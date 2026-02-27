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
    EditAssignees,
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

    // Column mode
    ToggleHiddenColumns,
    ToggleFocusedColumnHidden,
    ColRenameSelected,
    ColAddBefore,
    ColRemoveSelected,
    EnterColMoveMode,
    ColMoveLeft,
    ColMoveRight,
    ColMoveFirst,
    ColMoveLast,
    ColMoveToPosition(usize),

    // Search & filter
    StartFilter,
    StartTagFilter,
    StartAssigneeFilter,
    EnterFilterMode,
    ClearFilters,
    // Undo
    Undo,

    // Board
    ReloadBoard,
    ShowHelp,
    ShowMetrics,
    DismissTutorial,
    Quit,

    // Minor mode entry
    EnterGotoMode,
    EnterSpaceMode,
    EnterColumnMode,
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
