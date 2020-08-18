module Utility: {
  type intOption =
    | Some(int)
    | None;

  let switchUser: Board.Cell.symbol => Board.Cell.symbol;
  let safeConvToInt: string => intOption;
  let setCoor: (int, int) => Board.Cell.point;
  let getValueIntOption: intOption => int;
};

module Calc: {
  let isGridFinished: (Board.Cell.t, Board.BigBoard.t) => int;
  let isGameFinished: (Board.BoardCell.t, Board.Cell.symbol) => bool;
};
