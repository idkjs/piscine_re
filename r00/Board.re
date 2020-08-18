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
  let newCellPoint: (point, symbol) => t;
  let newCellCoor: (int, int, symbol) => t;

  let getCoordinates: t => point;
  let getSymbol: t => symbol;

  let symToString: symbol => string;
  let stringToSym: string => symbol;
} = {
  type symbol =
    | X
    | O
    | Empty
    | Space
    | Slash
    | Backslash
    | Vertical;

  type point = (int, int);

  type t = {
    coordinates: point,
    symbol,
  };

  let newPoint = (i, j) => (i, j); /*i = line / j = col */

  let newCellPoint = (p: point, s: symbol) => {coordinates: p, symbol: s};
  let newCellCoor = (i: int, j: int, s: symbol) => {
    coordinates: newPoint(i, j),
    symbol: s,
  };

  let getCoordinates = (cell: t) => cell.coordinates;
  let getSymbol = (cell: t) => cell.symbol;

  let symToString = (s: symbol) =>
    switch (s) {
    | X => "X"
    | O => "O"
    | Empty => "-"
    | Space => " "
    | Slash => "/"
    | Backslash => "\\"
    | Vertical => "|"
    };

  let stringToSym = (str: string) =>
    switch (str) {
    | "X" => X
    | "O" => O
    | "-" => Empty
    | _ => failwith("Error on string symbol")
    };
};

module BoardCell: {
  type t;

  let insertCell: (Cell.t, t) => t;
  let emptyBoardCell: unit => t;
  let winningBoard: Cell.symbol => t;
  let boardCellLineToString: (t, int) => string;
  let boardCellToString: t => string;
  let getCell: (Cell.point, t) => Cell.t;
  let isFull: t => bool;
  let fillMacroBoard: (t, int, Cell.symbol) => t;
  let filter: (Cell.t => bool, t) => list(Cell.t);
} = {
  type t = list(Cell.t);

  let emptyBoardCell = () => {
    let rec emptyBoardCell_aux = (cell: Cell.t, result: t) =>
      switch (Cell.getCoordinates(cell)) {
      | (i, j) when i == 2 && j == 2 => List.rev([cell, ...result])
      | (i, j) when j == 3 =>
        emptyBoardCell_aux(Cell.newCellCoor(i + 1, 0, Cell.Empty), result)
      | (i, j) =>
        emptyBoardCell_aux(
          Cell.newCellCoor(i, j + 1, Cell.Empty),
          [cell, ...result],
        )
      };
    emptyBoardCell_aux(Cell.newCellCoor(0, 0, Cell.Empty), []);
  };

  let newBoardO = () => {
    let rec loop = (coor: Cell.point, result: t) =>
      switch (coor) {
      | (i, j) when i == 3 => List.rev(result)
      | (i, j) when j == 3 => loop(Cell.newPoint(i + 1, 0), result)
      | (i, j) when i == j =>
        if (i == 1) {
          loop(
            Cell.newPoint(i, j + 1),
            [Cell.newCellPoint(coor, Cell.Space), ...result],
          );
        } else {
          loop(
            Cell.newPoint(i, j + 1),
            [Cell.newCellPoint(coor, Cell.Slash), ...result],
          );
        }
      | (i, j) when i == 1 =>
        loop(
          Cell.newPoint(i, j + 1),
          [Cell.newCellPoint(coor, Cell.Vertical), ...result],
        )
      | (i, j) when j == 1 =>
        loop(
          Cell.newPoint(i, j + 1),
          [Cell.newCellPoint(coor, Cell.Empty), ...result],
        )
      | (i, j) =>
        loop(
          Cell.newPoint(i, j + 1),
          [Cell.newCellPoint(coor, Cell.Backslash), ...result],
        )
      };
    loop(Cell.newPoint(0, 0), []);
  };

  let newBoardX = () => {
    let rec loop = (coor: Cell.point, result: t) =>
      switch (coor) {
      | (i, j) when i == 3 => List.rev(result)
      | (i, j) when j == 3 => loop(Cell.newPoint(i + 1, 0), result)
      | (i, j) when i == j =>
        if (i == 1) {
          loop(
            Cell.newPoint(i, j + 1),
            [Cell.newCellPoint(coor, Cell.X), ...result],
          );
        } else {
          loop(
            Cell.newPoint(i, j + 1),
            [Cell.newCellPoint(coor, Cell.Backslash), ...result],
          );
        }
      | (i, j) when (i, j) == (2, 0) || (i, j) == (0, 2) =>
        loop(
          Cell.newPoint(i, j + 1),
          [Cell.newCellPoint(coor, Cell.Slash), ...result],
        )
      | (i, j) =>
        loop(
          Cell.newPoint(i, j + 1),
          [Cell.newCellPoint(coor, Cell.Space), ...result],
        )
      };
    loop(Cell.newPoint(0, 0), []);
  };

  let winningBoard = (player: Cell.symbol) =>
    switch (player) {
    | Cell.O => newBoardO()
    | Cell.X => newBoardX()
    | _ =>
      failwith("Error in BoardCell.winningBoard -> player does not match any")
    };

  let insertCell = (cell: Cell.t, board: t) => {
    let rec insertCell_aux = (currentBoard: t, resultBoard: t) =>
      switch (currentBoard) {
      | [] =>
        raise(
          Failure(
            "Error in Board.BoardCell.insertCell : no place found for cell to be inserted",
          ),
        )
      | [h, ...tail]
          when Cell.getCoordinates(h) == Cell.getCoordinates(cell) =>
        List.rev([cell, ...resultBoard]) @ tail
      | [h, ...tail] => insertCell_aux(tail, [h, ...resultBoard])
      };

    insertCell_aux(board, []);
  };

  let rec isFull = (current_board: t) =>
    switch (current_board) {
    | [] => true
    | [h, ...t] when Cell.getSymbol(h) == Cell.Empty => false
    | [h, ...t] => isFull(t)
    };

  let boardCellLineToString = (board: t, line: int) => {
    let rec boardLineToString_aux = (currentBoard: t, result: string) =>
      switch (currentBoard) {
      | [] => result
      | [head, ...tail] =>
        switch (Cell.getCoordinates(head)) {
        | (i, j) when i == line =>
          if (j < 2) {
            boardLineToString_aux(
              tail,
              result ++ " " ++ Cell.symToString(Cell.getSymbol(head)),
            );
          } else {
            boardLineToString_aux(
              [],
              result ++ " " ++ Cell.symToString(Cell.getSymbol(head)),
            );
          }
        | _ => boardLineToString_aux(tail, result)
        }
      };
    boardLineToString_aux(board, "");
  };

  let boardCellToString = (board: t) => {
    let rec loop = (line: int, result: string) =>
      switch (line) {
      | x when x == 3 => Printf.sprintf("%s\n", result)
      | _ =>
        loop(
          line + 1,
          Printf.sprintf(
            "%s\n%s",
            result,
            boardCellLineToString(board, line),
          ),
        )
      };
    loop(0, "");
  };

  let rec getCell = (coor: Cell.point, boardCell: t) =>
    switch (boardCell) {
    | [] => failwith("Error : cell not found. getCell in Board.BoardCell")
    | [h, ...t] when Cell.getCoordinates(h) == coor => h
    | [h, ...t] => getCell(coor, t)
    };

  let fillMacroBoard = (board: t, boardIdx: int, s: Cell.symbol) => {
    let i = boardIdx / 3;
    let j = boardIdx mod 3;
    let cell = Cell.newCellCoor(i, j, s);
    insertCell(cell, board);
  };

  let filter = (ft: Cell.t => bool, board: t) => List.filter(ft, board);
};

module BigBoard: {
  type t;

  let boardLength: t => int;
  let emptyBigBoard: unit => t;
  let insertCell: (Cell.t, t) => t;
  let insertBoardCell: (BoardCell.t, int, t) => t;
  let boardToListString: t => list(string);
  let bigBoardToString: t => string;
  let getCell: (Cell.point, t) => Cell.t;
  let getBoardNumber: Cell.point => int;
  let getBoardCell: (Cell.point, t) => BoardCell.t;
  let isCellEmpty: (Cell.point, t) => bool;
  let isBoardCellAvail: (Cell.point, t, BoardCell.t) => bool;
} = {
  type t = list(BoardCell.t);

  let boardLength = (board: t) => List.length(board);

  let emptyBigBoard = () => {
    let rec loop = (n: int, result: t) =>
      switch (n) {
      | x when x == 9 => result
      | _ => loop(n + 1, [BoardCell.emptyBoardCell(), ...result])
      };
    loop(0, []);
  };

  let getBoardNumber = (coor: Cell.point) =>
    switch (coor) {
    | (i, j) when j < 3 && i < 3 => 0
    | (i, j) when j < 6 && i < 3 => 1
    | (i, j) when j < 9 && i < 3 => 2
    | (i, j) when j < 3 && i < 6 => 3
    | (i, j) when j < 6 && i < 6 => 4
    | (i, j) when j < 9 && i < 6 => 5
    | (i, j) when j < 3 && i < 9 => 6
    | (i, j) when j < 6 && i < 9 => 7
    | (i, j) when j < 9 && i < 9 => 8
    | _ => failwith("Error in getBoardNumber; Board module")
    };

  let convertCellLocal = (cell: Cell.t) => {
    let cellCoordinates = Cell.getCoordinates(cell);
    let newCoordinates =
      Cell.newPoint(fst(cellCoordinates) mod 3, snd(cellCoordinates) mod 3);
    Cell.newCellPoint(newCoordinates, Cell.getSymbol(cell));
  };

  let insertCell = (cell: Cell.t, bigBoard: t) => {
    let boardNumber = getBoardNumber(Cell.getCoordinates(cell));
    let cellToInsert = convertCellLocal(cell);
    let newBoardCell =
      BoardCell.insertCell(cellToInsert, List.nth(bigBoard, boardNumber));
    let buildBoardFun = (i: int, board: BoardCell.t) =>
      if (i == boardNumber) {
        newBoardCell;
      } else {
        board;
      };
    List.mapi(buildBoardFun, bigBoard);
  };

  let insertBoardCell = (boardCell: BoardCell.t, position: int, bigBoard: t) => {
    let matchIdx = (x: int, board: BoardCell.t) =>
      if (x == position) {
        boardCell;
      } else {
        board;
      };
    List.mapi(matchIdx, bigBoard);
  };

  let boardToListString = (bigBoard: t) => {
    let rec lineToString = (line: int, boardIdx: int, result: string) =>
      switch (boardIdx) {
      | x when x mod 3 == 2 =>
        Printf.sprintf(
          "%s%s",
          result,
          BoardCell.boardCellLineToString(
            List.nth(bigBoard, boardIdx),
            line,
          ),
        )
      | _ =>
        lineToString(
          line,
          boardIdx + 1,
          Printf.sprintf(
            "%s%s |",
            result,
            BoardCell.boardCellLineToString(
              List.nth(bigBoard, boardIdx),
              line,
            ),
          ),
        )
      };

    let rec listOfBlocLines =
            (line: int, boardIdx: int, result: list(string)) =>
      switch (line) {
      | x when x == 3 => List.rev(result)
      | _ =>
        listOfBlocLines(
          line + 1,
          boardIdx,
          [lineToString(line, boardIdx, ""), ...result],
        )
      };

    let rec allLines = (boardIdx: int, result: list(string)) =>
      switch (boardIdx) {
      | x when x == 6 =>
        let newBloc = listOfBlocLines(0, boardIdx, []);
        result @ newBloc;
      | _ =>
        let newBloc = listOfBlocLines(0, boardIdx, []);
        allLines(
          boardIdx + 3,
          result
          @ newBloc
          @ [String.make(String.length(List.hd(newBloc)), '-')],
        );
      };

    allLines(0, []);
  };

  let bigBoardToString = (bigBoard: t) => {
    let bigBoardListString = boardToListString(bigBoard);

    let rec loop = (boardString: list(string), result: string) =>
      switch (boardString) {
      | [] => result
      | [h, ...t] => loop(t, Printf.sprintf("%s%s\n", result, h))
      };

    loop(bigBoardListString, "\n");
  };

  let getCell = (coor: Cell.point, board: t) => {
    let boardNumber = getBoardNumber(coor);
    let localCellToFind =
      convertCellLocal(Cell.newCellPoint(coor, Cell.Empty));
    let localCell =
      BoardCell.getCell(
        Cell.getCoordinates(localCellToFind),
        List.nth(board, boardNumber),
      );
    Cell.newCellPoint(coor, Cell.getSymbol(localCell));
  };

  let getBoardCell = (coor: Cell.point, board: t) => {
    let boardNumber = getBoardNumber(coor);
    List.nth(board, boardNumber);
  };

  let isCellEmpty = (coor: Cell.point, board: t) => {
    let cell = getCell(coor, board);
    switch (Cell.getSymbol(cell)) {
    | Cell.Empty => true
    | _ => false
    };
  };

  let isBoardCellAvail = (coor: Cell.point, board: t, macroBoard: BoardCell.t) => {
    let boardNumber = getBoardNumber(coor);
    let coorInMacroBoard = Cell.newPoint(boardNumber / 3, boardNumber mod 3);
    let correspondingCellInMacro =
      BoardCell.getCell(coorInMacroBoard, macroBoard);
    if (Cell.getSymbol(correspondingCellInMacro) == Cell.Empty) {
      true;
    } else {
      false;
    };
  };
};
