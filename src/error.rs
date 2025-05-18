use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFile,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};

pub(crate) type SourceFile = SimpleFile<String, String>;

pub(crate) fn emit_error(file: &SourceFile, err: syn::Error) {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();
    let span = err.span().byte_range();
    let message = format!("{}", err);
    let diagnostic = Diagnostic::error()
        .with_message(&message)
        .with_labels(vec![Label::primary((), span).with_message(&message)]);
    term::emit(&mut writer.lock(), &config, file, &diagnostic).unwrap();
}

// #[cfg(test)]
// pub(crate) fn emit_errors_str<T: AsCodespanError>(
//     file: &SourceFile,
//     errs: impl Iterator<Item = T>,
// ) -> String {
//     use codespan_reporting::term::termcolor::Buffer;

//     let mut writer = Buffer::no_color();
//     let config = codespan_reporting::term::Config::default();
//     for err in errs {
//         let diagnostic = Diagnostic::error()
//             .with_message(err.message())
//             .with_labels(vec![
//                 Label::primary((), err.span()).with_message(err.message())
//             ]);
//         term::emit(&mut writer, &config, file, &diagnostic).unwrap();
//     }
//     String::from_utf8(writer.into_inner()).unwrap()
// }
