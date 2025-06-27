use tower_lsp::Client;
use tower_lsp::lsp_types::MessageType;
use tracing::field::Field;
use tracing::{Event, Level, Subscriber};
use tracing_subscriber::layer::{Context, Layer};
use tracing_subscriber::registry::LookupSpan;

pub struct LspLogLayer(Client);

impl LspLogLayer {
    pub fn new(client: Client) -> Self {
        LspLogLayer(client)
    }
}

impl<S> Layer<S> for LspLogLayer
where
    S: Subscriber + for<'a> LookupSpan<'a>,
{
    fn on_event(&self, event: &Event<'_>, _ctx: Context<'_, S>) {
        let level = *event.metadata().level();

        let message_type = match level {
            Level::ERROR => MessageType::ERROR,
            Level::WARN => MessageType::WARNING,
            Level::INFO => MessageType::INFO,
            Level::DEBUG => MessageType::LOG,
            _ => return,
        };

        // Extract the log message from the event
        let mut visitor = MessageVisitor::default();
        event.record(&mut visitor);
        let mut message = visitor.0;
        if message.is_empty() {
            message = format!("{event:?}");
        }

        tokio::spawn({
            let client = self.0.clone();

            async move {
                client.log_message(message_type, message).await;
            }
        });
    }
}

// Helper visitor to extract fields from the event
#[derive(Default)]
struct MessageVisitor(String);

impl tracing::field::Visit for MessageVisitor {
    fn record_debug(&mut self, field: &Field, value: &dyn std::fmt::Debug) {
        if field.name() == "message" {
            self.0 = format!("{value:?}");
        }
    }
}
