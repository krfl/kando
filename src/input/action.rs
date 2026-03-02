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
    PickPriority,
    MoveToColumn,
    ToggleBlocker,
    SetDueDate,
    ArchiveCard,
    PipeCard,
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
    ColSetWip,
    EnterColMoveMode,
    ColMoveLeft,
    ColMoveRight,
    ColMoveFirst,
    ColMoveLast,
    ColMoveToPosition(usize),

    // Sort
    StartSort,

    // Search & filter
    StartFilter,
    StartTagFilter,
    StartAssigneeFilter,
    StartStalenessFilter,
    StartOverdueFilter,
    EnterFilterMode,
    FindNext,
    FindPrev,
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
    InputCompleteForward,
    InputCompleteBackward,

    // Confirmation
    Confirm,
    Deny,

    // No-op
    None,
}
