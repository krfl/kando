use serde::{Deserialize, Serialize};

use crate::board::storage::{BoardSection, ColumnConfig};

#[derive(Debug, Serialize, Deserialize)]
pub struct BoardConfig {
    pub board: BoardSection,
    #[serde(rename = "columns")]
    pub columns: Vec<ColumnConfig>,
}
