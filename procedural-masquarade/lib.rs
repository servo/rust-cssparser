#[macro_export]
macro_rules! define_proc_macros {
    (
        $(
            $( #[$attr:meta] )*
            pub fn $func:ident($input:ident: &str) -> String
            $body:block
        )+
    ) => {
        $(
            $( #[$attr] )*
            #[proc_macro_derive($func)]
            pub fn $func(input: ::proc_macro::TokenStream) -> ::proc_macro::TokenStream {
                // Use another function to hide this one’s local variables from $block
                #[inline]
                fn implementation($input: &str) -> String $body

                let input = input.to_string();
                let mut normalized = String::with_capacity(input.len());
                for piece in input.split_whitespace() {
                    normalized.push_str(piece);
                    normalized.push(' ');
                }

                let prefix = "#[allow(unused)] enum Dummy { Input = (0, stringify!(";
                let suffix = ")).0, } ";
                assert!(normalized.starts_with(prefix), "Unexpected proc_macro_derive input {:?}", input);
                assert!(normalized.ends_with(suffix), "Unexpected proc_macro_derive input {:?}", input);

                let start = prefix.len();
                let end = normalized.len() - suffix.len();
                let output = implementation(&normalized[start..end]);
                output.parse().unwrap()
            }
        )+
    }
}

#[macro_export]
macro_rules! define_invoke_proc_macro {
    ($macro_name: ident) => {
        #[macro_export]
        macro_rules! $macro_name {
            ($proc_macro_name: ident ! $paren: tt) => {
                #[derive($proc_macro_name)]
                #[allow(unused)]
                enum Dummy {
                    Input = (0, stringify! $paren ).0
                }
            }
        }
    }
}
