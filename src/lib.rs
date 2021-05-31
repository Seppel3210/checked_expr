#![warn(clippy::all)]
#![warn(clippy::cargo)]
#![warn(clippy::pedantic)]
//! Crate providing a procedural macro to make normal operations into overflow checked operations easily
extern crate proc_macro;

use proc_macro2::{Ident, Span, TokenStream, TokenTree};
use quote::quote;
use syn::{parse_macro_input, BinOp, Expr, ExprBinary, ExprMethodCall, ExprUnary, UnOp};

/// Procedural macro to convert normal integer operations into overflow-checked operations.
/// The expression will evaluate to `Some(result)` if all operations succed or to `None` if any of
/// the operations fails.
///
/// This Macro supports checked functions for these operators:
/// `-` (`neg`),`+` (`add`), `-` (`sub`), `*` (`mul`), `/` (`div`), `%` (`rem`), `<<` (`shl`), `>>` (`shr`)
/// And it also substitutes these methods with their checked variants:
/// `abs`, `pow`
///
/// # Examples
/// ```
/// use checked_expr::checked_expr;
/// assert_eq!(checked_expr!(254_u8 + 1), Some(255));
/// assert_eq!(checked_expr!(255_u8 + 1), None);
///
/// assert_eq!(checked_expr!(-(-127_i8)), Some(127));
/// assert_eq!(checked_expr!(-(-128_i8)), None);
///
/// // also works with `pow` and `abs`
/// assert_eq!(checked_expr!((-128_i8).abs()), None);
/// assert_eq!(checked_expr!(10_u16.pow(100)), None);
///
/// // you can also arbitrarily nest expressions although you sometimes need to be very
/// // explicit with the types on literals on the left hand side of operations
/// assert_eq!(checked_expr!((10_i32 - 8) * (3_i32 + 6) % 8), Some(2));
/// ```
#[proc_macro]
pub fn checked_expr(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // the idea is to convert something like this
    // 1 + 2 * 3
    // into
    // (|| Some(1.checked_add(2.checked_mul(3)?)?) )()
    let input = parse_macro_input!(input as Expr);

    let expr = expr(&input);
    let tokens = quote! {
        (|| Some(#expr) )()
    };
    tokens.into()
}

fn expr(expr: &Expr) -> TokenStream {
    match expr {
        Expr::Unary(expr) => unary_expr(expr),
        Expr::Binary(expr) => binary_expr(expr),
        Expr::Paren(expr) => crate::expr(&expr.expr),
        Expr::MethodCall(expr) => method_call(expr),
        _ => quote! {
            (#expr)
        },
    }
}

fn binary_expr(expr: &ExprBinary) -> TokenStream {
    let function_name = match expr.op {
        BinOp::Add(_) => "checked_add",
        BinOp::Sub(_) => "checked_sub",
        BinOp::Mul(_) => "checked_mul",
        BinOp::Div(_) => "checked_div",
        BinOp::Rem(_) => "checked_rem",
        BinOp::Shl(_) => "checked_shl",
        BinOp::Shr(_) => "checked_shr",
        _ => {
            return quote! {
                (#expr)
            }
        }
    };
    let fn_ident = TokenTree::Ident(Ident::new(function_name, Span::call_site()));

    let left = crate::expr(&expr.left);
    let right = crate::expr(&expr.right);
    quote! {
        #left.#fn_ident(#right)?
    }
}

fn unary_expr(expr: &ExprUnary) -> TokenStream {
    let function_name = match expr.op {
        UnOp::Neg(_) => "checked_neg",
        _ => {
            return quote! {
                (#expr)
            }
        }
    };
    let fn_ident = TokenTree::Ident(Ident::new(function_name, Span::call_site()));

    let inner = crate::expr(&expr.expr);
    quote! {
        #inner.#fn_ident()?
    }
}

fn method_call(expr: &ExprMethodCall) -> TokenStream {
    let method_name = match expr.method.to_string().as_str() {
        "abs" => "checked_abs",
        "pow" => "checked_pow",
        _ => {
            return quote! {
                (#expr)
            }
        }
    };
    let method_ident = TokenTree::Ident(Ident::new(method_name, Span::call_site()));
    let left = crate::expr(&expr.receiver);
    let args = &expr.args;
    quote! {
        #left.#method_ident(#args)?
    }
}
