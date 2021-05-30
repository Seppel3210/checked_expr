//! Crate providing a procedural macro to make normal operations into overflow checked operations easily
extern crate proc_macro;

use syn::{parse_macro_input, Expr, ExprBinary, ExprUnary, BinOp, UnOp};
use quote::{quote, ToTokens};
use proc_macro2::{TokenTree, Ident, Span, TokenStream};

/// Procedural macro to convert normal integer operations into overflow-checked operations.
/// The expression will evaluate to `Some(result)` if all operations succed or to `None` if any of
/// the operations fails.
/// # Examples
/// ```
/// use checked_expr::checked_expr;
/// assert_eq!(checked_expr!(254_u8 + 1), Some(255));
/// assert_eq!(checked_expr!(255_u8 + 1), None);
///
/// // this even works on negation
/// assert_eq!(checked_expr!(-(-127 as i8)), Some(127));
/// assert_eq!(checked_expr!(-(-128 as i8)), None);
///
/// // you can also arbitrarily nest expressions although you sometimes need to be very
/// // explicit with the types on literals on the left hand side of operations
/// assert_eq!(checked_expr!((10_i32 - 8) * (40_i32 + 13) / 8), Some(13));
/// ```
#[proc_macro]
pub fn checked_expr(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as Expr);

    expr(&input).into()
}

fn expr(expr: &Expr) -> TokenStream {
    match expr {
        Expr::Unary(expr) => { unary_expr(expr) }
        Expr::Binary(expr) => { binary_expr(expr) }
        Expr::Paren(expr) => crate::expr(&expr.expr),
        _ => {
            let tokens = quote! {
                Some(#expr)
            };
            tokens.into()
        }
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
        _ => return expr.to_token_stream()
    };
    let fn_ident = TokenTree::Ident(Ident::new(function_name, Span::call_site()));

    let left = crate::expr(&expr.left);
    let right = crate::expr(&expr.right);
    quote! {
        #left.and_then(|x| x.#fn_ident(#right?))
    }
}

fn unary_expr(expr: &ExprUnary) -> TokenStream {
    let function_name = match expr.op {
        UnOp::Neg(_) => "checked_neg",
        _ => return expr.to_token_stream()
    };
    let fn_ident = TokenTree::Ident(Ident::new(function_name, Span::call_site()));

    let inner = crate::expr(&expr.expr);
    quote! {
        #inner.and_then(|x| x.#fn_ident())
    }
}