pub mod board;
pub mod config;

// Curated re-exports for ergonomic access.
pub use board::{Board, Card, Column, Policies, Priority, Template};
pub use board::storage::{BoardContext, BoardMode};
pub use config::{BoardConfig, KandoToml, LocalConfig};
