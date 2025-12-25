use quick_xml::events::Event;
use quick_xml::reader::Reader;

use crate::models::NyaaItem;
use crate::RssError;

/// Parse a Nyaa RSS feed from raw XML bytes
pub fn parse_nyaa_feed(xml: &[u8]) -> Result<Vec<NyaaItem>, RssError> {
    let mut reader = Reader::from_reader(xml);
    reader.config_mut().trim_text(true);

    let mut items = Vec::new();
    let mut buf = Vec::new();

    let mut current_item: Option<NyaaItemBuilder> = None;
    let mut current_element = String::new();

    loop {
        match reader.read_event_into(&mut buf) {
            Ok(Event::Start(e)) => {
                let name = String::from_utf8_lossy(e.name().as_ref()).to_string();
                current_element = name.clone();

                if name == "item" {
                    current_item = Some(NyaaItemBuilder::default());
                }
            }
            Ok(Event::End(e)) => {
                let name = String::from_utf8_lossy(e.name().as_ref()).to_string();

                if name == "item" {
                    if let Some(builder) = current_item.take() {
                        if let Some(item) = builder.build() {
                            items.push(item);
                        }
                    }
                }
                current_element.clear();
            }
            Ok(Event::Text(e)) => {
                if let Some(ref mut item) = current_item {
                    let text = e.unescape().unwrap_or_default().to_string();
                    if !text.is_empty() {
                        match current_element.as_str() {
                            "title" => item.title = Some(text),
                            "link" => item.torrent_url = Some(text),
                            "nyaa:infoHash" => item.info_hash = Some(text.to_lowercase()),
                            _ => {}
                        }
                    }
                }
            }
            Ok(Event::Eof) => break,
            Err(e) => return Err(RssError::Parse(format!("XML parse error: {}", e))),
            _ => {}
        }
        buf.clear();
    }

    Ok(items)
}

#[derive(Default)]
struct NyaaItemBuilder {
    title: Option<String>,
    torrent_url: Option<String>,
    info_hash: Option<String>,
}

impl NyaaItemBuilder {
    fn build(self) -> Option<NyaaItem> {
        Some(NyaaItem {
            title: self.title?,
            torrent_url: self.torrent_url?,
            info_hash: self.info_hash?,
        })
    }
}
