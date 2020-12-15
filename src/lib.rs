use ansi_term::{
    Color::{Blue, Cyan, Green, Purple, Red, Yellow},
    Style,
};
use indent_write::fmt::IndentWriter;
use once_cell::sync::Lazy;
use std::{
    fmt::{self, Write},
    sync::Mutex,
};
use tracing_core::{field::Visit, span::Id, Event, Field, Level, Subscriber};
use tracing_subscriber::{
    field::RecordFields,
    fmt::{
        time::{FormatTime, SystemTime},
        FmtContext, FormatEvent, FormatFields, FormattedFields,
    },
    registry::LookupSpan,
};

static BOLD: Lazy<Style> = Lazy::new(|| Style::new().bold());
static DIMMED: Lazy<Style> = Lazy::new(|| Style::new().dimmed());

#[derive(Debug, Default)]
pub struct FmtEvent<T = SystemTime> {
    timer: T,
    spans: Mutex<Vec<Id>>,
}

#[derive(Debug, Default)]
pub struct FmtFields {
    _reserved: (),
}

impl FmtEvent<SystemTime> {
    pub fn new() -> Self {
        Self::default()
    }
}

impl FmtFields {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<T> FmtEvent<T> {
    pub fn with_timer<T2>(self, timer: T2) -> FmtEvent<T2> {
        FmtEvent {
            timer,
            spans: self.spans,
        }
    }

    pub fn without_time(self) -> FmtEvent<()> {
        FmtEvent {
            timer: (),
            spans: self.spans,
        }
    }
}

struct MessageVisitor<'a>(&'a mut dyn fmt::Write);

impl<'a> Visit for MessageVisitor<'a> {
    fn record_debug(&mut self, field: &Field, value: &dyn fmt::Debug) {
        if field.name() == "message" {
            write!(self.0, " {:?}", value).unwrap();
        }
    }
}

fn make_indentation(indent: usize) -> String {
    format!("{:1$}", "", indent)
}

struct FmtLevel(Level);

impl fmt::Display for FmtLevel {
    #[fehler::throws(fmt::Error)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) {
        match self.0 {
            Level::TRACE => write!(f, "{}", Purple.paint("TRCE"))?,
            Level::DEBUG => write!(f, "{}", Blue.paint("DEBG"))?,
            Level::INFO => write!(f, "{}", Green.paint("INFO"))?,
            Level::WARN => write!(f, "{}", Yellow.paint("WARN"))?,
            Level::ERROR => write!(f, "{}", Red.paint("EROR"))?,
        }
    }
}

impl<S, N, T> FormatEvent<S, N> for FmtEvent<T>
where
    S: Subscriber + for<'a> LookupSpan<'a>,
    N: for<'a> FormatFields<'a> + 'static,
    T: FormatTime,
{
    #[fehler::throws(fmt::Error)]
    fn format_event(
        &self,
        ctx: &FmtContext<'_, S, N>,
        mut writer: &mut dyn Write,
        event: &Event<'_>,
    ) {
        let mut spans = self.spans.lock().unwrap();
        while !spans.last().map_or(true, |id| ctx.exists(id)) {
            spans.pop();
        }
        for (indent, span) in ctx.scope().enumerate().skip(spans.len()) {
            let indentation = make_indentation(indent * 2);
            let mut writer = IndentWriter::new(&indentation, &mut writer);
            let metadata = span.metadata();
            let ext = span.extensions();
            let fields = ext
                .get::<FormattedFields<N>>()
                .expect("Unable to find FormattedFields in extensions; this is a bug");
            write!(
                writer,
                "{}   [1m{}::{}[0m\n{}",
                DIMMED.paint("in"),
                BOLD.paint(metadata.target()),
                BOLD.fg(Cyan).paint(metadata.name()),
                fields
            )?;
            spans.push(span.id());
        }
        let metadata = event.metadata();

        let mut time = String::new();
        self.timer.format_time(&mut time)?;
        if !time.is_empty() {
            time.push(' ');
        }

        let indentation = make_indentation(spans.len() * 2);
        let mut writer = IndentWriter::new(&indentation, &mut writer);
        write!(
            writer,
            "{} {}{}::{}",
            FmtLevel(*metadata.level()),
            time,
            BOLD.paint(metadata.target()),
            BOLD.fg(Cyan).paint(metadata.name())
        )?;
        event.record(&mut MessageVisitor(&mut writer));
        writeln!(writer)?;
        ctx.field_format().format_fields(&mut writer, event)?;
    }
}

struct FieldsVisitor<'a>(&'a mut dyn fmt::Write, bool);

impl<'a> Visit for FieldsVisitor<'a> {
    fn record_debug(&mut self, field: &Field, value: &dyn fmt::Debug) {
        if field.name() == "message" {
            return;
        }
        write!(
            self.0,
            "{} [1m{}[0m: ",
            DIMMED.paint(if self.1 { "with" } else { "    " }),
            BOLD.paint(field.name())
        )
        .unwrap();
        let mut writer = IndentWriter::new("     ", &mut self.0);
        writer.skip_next_indent();
        writeln!(writer, "{:#?}", value).unwrap();
        self.1 = false;
    }
}

impl<'writer> FormatFields<'writer> for FmtFields {
    #[fehler::throws(fmt::Error)]
    fn format_fields<R: RecordFields>(&self, writer: &'writer mut dyn Write, fields: R) {
        fields.record(&mut FieldsVisitor(writer, true));
        writeln!(writer)?;
    }
}
