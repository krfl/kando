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
    EnterCommandMode,

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
    InputComplete,
    InputCompleteBack,

    // Confirmation
    Confirm,
    Deny,

    // No-op
    None,
}
