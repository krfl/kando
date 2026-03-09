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
    JumpToFirstCard,
    JumpToLastCard,
    JumpToFirstCardGlobal,
    JumpToLastCardGlobal,
    JumpToFirstColumn,
    JumpToLastColumn,
    JumpToColumnByLetter(char),

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
    // Undo / Repeat
    Undo,
    RepeatLast,

    // Board
    ReloadBoard,
    ShowHelp,
    ToggleHelpPage,
    ShowMetrics,
    Quit,

    // Template actions
    EnterTemplateMode,
    TemplateNew,
    TemplateEdit,
    TemplateDelete,
    TemplateRename,

    // Minor mode entry
    EnterGotoMode,
    EnterGotoColumnMode,
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
