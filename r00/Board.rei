module Cell: {
  type symbol =
    | X
    | O
    | Empty
    | Space
    | Slash
    | Backslash
    | Vertical;
  type point = (int, int);
  type t;

  let newPoint: (int, int) => point;
  let newCellCoor: (int, int, symbol) => t;
  let newCellPoint: (point, symbol) => t;
  let stringToSym: string => symbol;
  let symToString: symbol => string;
  let getCoordinates: t => point;
  let getSymbol: t => symbol;
};

module BoardCell: {
  type t;

  let emptyBoardCell: unit => t;
  let winningBoard: Cell.symbol => t;
  let insertCell: (Cell.t, t) => t;
  let boardCellToString: t => string;
  let fillMacroBoard: (t, int, Cell.symbol) => t;
  let isFull: t => bool;
  let filter: (Cell.t => bool, t) => list(Cell.t);
};

module BigBoard: {
  type t;

  let boardLength: t => int;
  let emptyBigBoard: unit => t;
  let insertCell: (Cell.t, t) => t;
  let insertBoardCell: (BoardCell.t, int, t) => t;
  let boardToListString: t => list(string);
  let bigBoardToString: t => string;
  let isCellEmpty: (Cell.point, t) => bool;
  let isBoardCellAvail: (Cell.point, t, BoardCell.t) => bool;
  let getBoardCell: (Cell.point, t) => BoardCell.t;
  let getBoardNumber: Cell.point => int;
};
