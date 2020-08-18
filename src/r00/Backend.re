module Utility: {
  type intOption =
    | Some(int)
    | None;

  let switchUser: Board.Cell.symbol => Board.Cell.symbol;
  let safeConvToInt: string => intOption;
  let setCoor: (int, int) => Board.Cell.point;
  let getValueIntOption: intOption => int;
} = {
  type intOption =
    | Some(int)
    | None;

  let getValueIntOption = x =>
    switch (x) {
    | Some(n) => n
    | None =>
      failwith(
        "Error in conception : getValueIntOption should not be called on None",
      )
    };

  let switchUser = s =>
    switch (s) {
    | Board.Cell.X => Board.Cell.O
    | Board.Cell.O => Board.Cell.X
    | _ => failwith("Error in switchUser : Unknown user")
    };

  /* let safeConvToInt (str:string) =
     try Some (int_of_string str)
     with Failure "int_of_string" -> None */

  let safeConvToInt = (str: string) =>
    switch (int_of_string(str)) {
    | exception (Failure(_)) => None
    | _ => Some(int_of_string(str))
    };

  let setCoor = (x, y) => Board.Cell.newPoint(x - 1, y - 1);
};

module Calc: {
  let checkListSymbol: (list(Board.Cell.t), Board.Cell.symbol) => bool;

  let isGridFinished: (Board.Cell.t, Board.BigBoard.t) => int;
  let isGameFinished: (Board.BoardCell.t, Board.Cell.symbol) => bool;
} = {
  let extractLns = (board: Board.BoardCell.t) => {
    let rec loop = (line: int, result: list(list(Board.Cell.t))) =>
      switch (line) {
      | 3 => result
      | _ =>
        let filterLines = (cell: Board.Cell.t) => {
          let (i, j) = Board.Cell.getCoordinates(cell);
          if (i == line) {
            true;
          } else {
            false;
          };
        };

        loop(
          line + 1,
          [Board.BoardCell.filter(filterLines, board), ...result],
        );
      };

    loop(0, []);
  };

  let extractCols = (board: Board.BoardCell.t) => {
    let rec loop = (cols: int, result: list(list(Board.Cell.t))) =>
      switch (cols) {
      | 3 => result
      | _ =>
        let filterCols = (cell: Board.Cell.t) => {
          let (i, j) = Board.Cell.getCoordinates(cell);
          if (j == cols) {
            true;
          } else {
            false;
          };
        };

        loop(
          cols + 1,
          [Board.BoardCell.filter(filterCols, board), ...result],
        );
      };

    loop(0, []);
  };

  let extractDiagos = (board: Board.BoardCell.t) => {
    let filterDiago1 = (cell: Board.Cell.t) => {
      let (i, j) = Board.Cell.getCoordinates(cell);
      if (i == j) {
        true;
      } else {
        false;
      };
    };

    let filterDiago2 = (cell: Board.Cell.t) => {
      let (i, j) = Board.Cell.getCoordinates(cell);
      if (i + j == 2) {
        true;
      } else {
        false;
      };
    };

    let list1 = Board.BoardCell.filter(filterDiago1, board);
    let list2 = Board.BoardCell.filter(filterDiago2, board);
    [list1, list2];
  };

  let checkListSymbol = (lst: list(Board.Cell.t), s: Board.Cell.symbol) => {
    let rec loop = current_list =>
      switch (current_list) {
      | [] => true
      | [h, ...t] =>
        if (Board.Cell.getSymbol(h) == s) {
          loop(t);
        } else {
          false;
        }
      };
    loop(lst);
  };

  let isWon = (board: Board.BoardCell.t, s: Board.Cell.symbol) => {
    let listToCheck =
      extractLns(board) @ extractCols(board) @ extractDiagos(board);
    let rec loop = (lst: list(list(Board.Cell.t))) =>
      switch (lst) {
      | [] => false
      | [h, ...t] =>
        if (checkListSymbol(h, s) == true) {
          true;
        } else {
          loop(t);
        }
      };

    loop(listToCheck);
  };

  let isGridFinished = (cell: Board.Cell.t, board: Board.BigBoard.t) => {
    let boardNumber =
      Board.BigBoard.getBoardNumber(Board.Cell.getCoordinates(cell));
    let current_board =
      Board.BigBoard.getBoardCell(Board.Cell.getCoordinates(cell), board);
    if (isWon(current_board, Board.Cell.getSymbol(cell))
        || Board.BoardCell.isFull(current_board)) {
      boardNumber;
    } else {
      (-1);
    };
  };

  let isGameFinished =
      (macroBoard: Board.BoardCell.t, player: Board.Cell.symbol) =>
    if (isWon(macroBoard, player) || Board.BoardCell.isFull(macroBoard)) {
      true;
    } else {
      false;
    };
};
