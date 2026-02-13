/// All possible semantic actions in Kando.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    // Navigation
    FocusPrevColumn,
    FocusNextColumn,
    SelectPrevCard,
    SelectNextCard,
    CycleNextCard,
    CyclePrevCard,
    JumpToColumn(usize),
    JumpToFirstCard,
    JumpToLastCard,
    JumpToBacklog,
    JumpToDone,

    // Card movement
    MoveCardPrevColumn,
    MoveCardNextColumn,

    // Card actions
    NewCard,
    DeleteCard,
    EditCardExternal,
    EditTags,
    CyclePriority,
    PickPriority,
    MoveToColumn,
    ToggleBlocker,
    OpenCardDetail,
    ClosePanel,
    DetailScrollUp,
    DetailScrollDown,
    DetailNextCard,
    DetailPrevCard,

    // View
    ToggleHiddenColumns,

    // Search & filter
    StartFilter,
    StartTagFilter,
    ClearFilters,
    // Board
    ReloadBoard,
    ShowHelp,
    DismissTutorial,
    Quit,

    // Minor mode entry
    EnterGotoMode,
    EnterSpaceMode,
    EnterViewMode,

    // Input modal
    InputConfirm,
    InputCancel,
    InputChar(char),
    InputBackspace,
    InputLeft,
    InputRight,
    InputHome,
    InputEnd,
    InputDeleteWord,

    // Confirmation
    Confirm,
    Deny,

    // No-op
    None,
}
