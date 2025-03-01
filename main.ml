(* Тип для представления нонограммы *)
type nonogram = {
  rows : int list list;  (* Подсказки для строк *)
  cols : int list list;  (* Подсказки для столбцов *)
  grid : bool option array array;  (* Сетка: None - неизвестно, Some true - закрашено, Some false - пусто *)

}

(* Создание пустой сетки *)
let create_grid rows cols =
  Array.make_matrix (List.length rows) (List.length cols) None

(* Генерация случайной нонограммы *)
let generate_nonogram size =
  let solution = Array.make_matrix size size false in
  (* Заполняем случайные клетки *)
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      solution.(i).(j) <- Random.bool ()
    done
  done;
  (* Генерируем подсказки для строк и столбцов *)
  let get_hints arr =
    let rec count acc = function
      | [] -> List.rev (if acc > 0 then [acc] else [])
      | false :: t -> count (if acc > 0 then acc else 0) t
      | true :: t -> count (acc + 1) t
    in
    Array.to_list arr |> count 0
  in
  let rows = Array.map (fun row -> get_hints row) solution |> Array.to_list in
  let cols = Array.init size (fun j ->
    get_hints (Array.init size (fun i -> solution.(i).(j))))
    |> Array.to_list
  in
  {
    rows;
    cols;
    grid = create_grid rows cols;

  }
(* Очистка экрана *)
let clear_screen () =
  ignore (Sys.command "clear")  (* Для Unix-систем *)
  (* Для Windows замените "clear" на "cls" *)

(* Вывод сетки с подсказками и курсором *)
let print_nonogram nonogram cursor_x cursor_y =
  clear_screen ();  (* Очищаем экран перед выводом новой сетки *)
  (* Вывод подсказок для столбцов *)
  let max_col_hints = List.fold_left (max) 0 (List.map List.length nonogram.cols) in
  for i = max_col_hints - 1 downto 0 do
    print_string "   ";
    List.iter (fun col ->
      let hint = try List.nth col i with _ -> 0 in
      Printf.printf "%2d " hint
    ) nonogram.cols;
    print_newline ()
  done;
  (* Вывод сетки и подсказок для строк *)
  Array.iteri (fun i row ->
    List.iter (fun hint -> Printf.printf "%2d " hint) (List.nth nonogram.rows i);
    Array.iteri (fun j cell ->
      if i = cursor_x && j = cursor_y then print_string "[";
      match cell with
      | None -> print_string " . "
      | Some true -> print_string " # "
      | Some false -> print_string " x ";
      if i = cursor_x && j = cursor_y then print_string "]"
    ) row;
    print_newline ()
  ) nonogram.grid

(* Функция для проверки, соответствует ли строка или столбец подсказкам *)
let matches_hints arr hints =
  let rec count_groups acc = function
    | [] -> List.rev (if acc > 0 then [acc] else [])
    | Some false :: t -> count_groups (if acc > 0 then acc else 0) t
    | Some true :: t -> count_groups (acc + 1) t
    | None :: t -> count_groups acc t  (* Игнорируем пустые клетки *)
  in
  let groups = count_groups 0 arr in
  groups = hints

(* Проверка всех строк *)
let check_rows nonogram =
  let rows_array = Array.of_list nonogram.rows in
  Array.for_all2 (fun row hints ->
    matches_hints (Array.to_list row) hints
  ) nonogram.grid rows_array

(* Проверка всех столбцов *)
let check_cols nonogram =
  let cols = Array.init (Array.length nonogram.grid.(0)) (fun j ->
    Array.init (Array.length nonogram.grid) (fun i -> nonogram.grid.(i).(j))
  ) in
  let cols_hints_array = Array.of_list nonogram.cols in
  Array.for_all2 (fun col hints ->
    matches_hints (Array.to_list col) hints
  ) cols cols_hints_array
(* Общая проверка решения *)
let check_solution nonogram =
  check_rows nonogram && check_cols nonogram

(* Основной цикл игры с управлением WASD и -+ *)
let play_nonogram nonogram =
  let size = Array.length nonogram.grid in
  let cursor_x = ref 0 in
  let cursor_y = ref 0 in
  let rec play () =
    print_nonogram nonogram !cursor_x !cursor_y;
    print_endline "Используйте WASD для перемещения, '-' для очистки, '+' для закрашивания.";
    print_endline "Нажмите 'q' для выхода.";
    if check_solution nonogram then (
      print_nonogram nonogram !cursor_x !cursor_y;
      print_endline "Поздравляем! Вы решили нонограмму!";
      exit 0;
    );
    let input = read_line () in
    match input with
    | "w" -> if !cursor_x > 0 then decr cursor_x; play ()
    | "s" -> if !cursor_x < size - 1 then incr cursor_x; play ()
    | "a" -> if !cursor_y > 0 then decr cursor_y; play ()
    | "d" -> if !cursor_y < size - 1 then incr cursor_y; play ()
    | "+" -> nonogram.grid.(!cursor_x).(!cursor_y) <- Some true; play ()
    | "-" -> nonogram.grid.(!cursor_x).(!cursor_y) <- Some false; play ()
    | "q" -> print_endline "Выход из игры."; exit 0
    | _ -> print_endline "Некорректный ввод. Попробуйте снова."; play ()
  in
  play ()

(* Запуск игры *)
let () =
  Random.self_init ();
  print_endline "Введите размер нонограммы (например, 5 для 5x5):";
  let size = read_int () in
  if size < 1 then (
    print_endline "Размер должен быть положительным числом.";
    exit 1;
  );
  let nonogram = generate_nonogram size in
  print_endline "Добро пожаловать в игру Нонограмма!";
  play_nonogram nonogram