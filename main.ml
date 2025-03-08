(* Тип для представления нонограммы *)
type nonogram = {
  rows : int list list;  (* Подсказки для строк *)
  cols : int list list;  (* Подсказки для столбцов *)
  grid : bool option array array;  (* Сетка: Some true - закрашено, Some false - пусто *)
  solution : bool array array;  (* Решение нонограммы *)
}

(* Создание пустой сетки, заполненной Some false *)
let create_grid rows cols =
  Array.make_matrix (List.length rows) (List.length cols) (Some false)

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
      | false :: t ->
          if acc > 0 then
            count 0 t |> (fun groups -> acc :: groups)
          else
            count 0 t
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
    solution;  (* Сохраняем решение *)
  }

(* Очистка экрана *)
let clear_screen () =
  ignore (Sys.command "clear")  (* Для Unix-систем *)
  (* Для Windows замените "clear" на "cls" *)

(* Вывод сетки с подсказками и курсором *)
let print_nonogram nonogram cursor_x cursor_y =
  clear_screen ();  (* Очищаем экран перед выводом новой сетки *)

  (* Вычисляем максимальное количество подсказок для строк *)
  let max_row_hints = List.fold_left (max) 0 (List.map List.length nonogram.rows) in

  (* Вывод подсказок для столбцов *)
  let max_col_hints = List.fold_left (max) 0 (List.map List.length nonogram.cols) in
  for i = max_col_hints - 1 downto 0 do
    (* Отступ для подсказок строк *)
    for _ = 1 to max_row_hints * 3 do
      print_string " "
    done;
    (* Вывод подсказок для столбцов с разделением по клеткам *)
    List.iter (fun col ->
      let hint = try List.nth col i with _ -> 0 in
      Printf.printf "|%2d " hint
    ) nonogram.cols;
    print_string "|";  (* Закрываем последнюю ячейку строки *)
    print_newline ()
  done;

  (* Вывод горизонтальной линии для разделения подсказок и сетки *)
  for _ = 1 to max_row_hints * 3 do
    print_string " "
  done;
  List.iter (fun _ -> print_string "+---") nonogram.cols;
  print_string "+";  (* Закрываем последнюю ячейку строки *)
  print_newline ();

  (* Вывод сетки и подсказок для строк *)
  Array.iteri (fun i row ->
    (* Вывод подсказок для строк *)
    let row_hints = List.nth nonogram.rows i in
    let padding = max_row_hints - List.length row_hints in
    for _ = 1 to padding * 3 do
      print_string " "
    done;
    List.iter (fun hint -> Printf.printf "%2d " hint) row_hints;

    (* Вывод ячеек сетки с границами *)
    Array.iteri (fun j cell ->
      print_string "|";
      if i = cursor_x && j = cursor_y then (
        (* Выделяем ячейку курсора *)
        match cell with
        | Some true -> print_string ">#<"
        | Some false -> print_string ">x<"
        | None -> print_string " . "
      ) else (
        (* Обычный вывод ячейки *)
        match cell with
        | Some true -> print_string " # "
        | Some false -> print_string " x "
        | None -> print_string " . "
      )
    ) row;
    print_string "|";  (* Закрываем последнюю ячейку строки *)
    print_newline ();

    (* Вывод горизонтальной линии между строками *)
    for _ = 1 to max_row_hints * 3 do
      print_string " "
    done;
    List.iter (fun _ -> print_string "+---") nonogram.cols;
    print_string "+";  (* Закрываем последнюю ячейку строки *)
    print_newline ()
  ) nonogram.grid

(* Функция для проверки, соответствует ли строка или столбец подсказкам *)
let matches_hints arr hints =
  let rec count_groups acc = function
    | [] -> List.rev (if acc > 0 then [acc] else [])
    | Some false :: t ->
        if acc > 0 then
          count_groups 0 t |> (fun groups -> acc :: groups)
        else
          count_groups 0 t
    | Some true :: t -> count_groups (acc + 1) t
    | None :: t -> count_groups acc t
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

(* Функция для отображения решения нонограммы *)
let show_solution nonogram =
  clear_screen ();  (* Очищаем экран перед выводом решения *)

  (* Вычисляем максимальное количество подсказок для строк *)
  let max_row_hints = List.fold_left (max) 0 (List.map List.length nonogram.rows) in

  (* Вывод подсказок для столбцов *)
  let max_col_hints = List.fold_left (max) 0 (List.map List.length nonogram.cols) in
  for i = max_col_hints - 1 downto 0 do
    (* Отступ для подсказок строк *)
    for _ = 1 to max_row_hints * 3 do
      print_string " "
    done;
    (* Вывод подсказок для столбцов с разделением по клеткам *)
    List.iter (fun col ->
      let hint = try List.nth col i with _ -> 0 in
      Printf.printf "|%2d " hint
    ) nonogram.cols;
    print_string "|";  (* Закрываем последнюю ячейку строки *)
    print_newline ()
  done;

  (* Вывод горизонтальной линии для разделения подсказок и сетки *)
  for _ = 1 to max_row_hints * 3 do
    print_string " "
  done;
  List.iter (fun _ -> print_string "+---") nonogram.cols;
  print_string "+";  (* Закрываем последнюю ячейку строки *)
  print_newline ();

  (* Вывод сетки с решением *)
  Array.iteri (fun i row ->
    (* Вывод подсказок для строк *)
    let row_hints = List.nth nonogram.rows i in
    let padding = max_row_hints - List.length row_hints in
    for _ = 1 to padding * 3 do
      print_string " "
    done;
    List.iter (fun hint -> Printf.printf "%2d " hint) row_hints;

    (* Вывод ячеек сетки с границами *)
    Array.iteri (fun j _ ->
      print_string "|";
      if nonogram.solution.(i).(j) then
        print_string " # "  (* Закрашенные ячейки *)
      else
        print_string " x "  (* Пустые ячейки *)
    ) row;
    print_string "|";  (* Закрываем последнюю ячейку строки *)
    print_newline ();

    (* Вывод горизонтальной линии между строками *)
    for _ = 1 to max_row_hints * 3 do
      print_string " "
    done;
    List.iter (fun _ -> print_string "+---") nonogram.cols;
    print_string "+";  (* Закрываем последнюю ячейку строки *)
    print_newline ()
  ) nonogram.solution;

  print_endline "Это решение нонограммы. Нажмите Enter, чтобы продолжить.";
  ignore (read_line ())  (* Ждём нажатия Enter *)
(* Основной цикл игры с управлением WASD, -+, и show *)
let play_nonogram nonogram =
  let size = Array.length nonogram.grid in
  let rec play cursor_x cursor_y grid =
    print_nonogram { nonogram with grid } cursor_x cursor_y;
    print_endline "Используйте WASD для перемещения, '-' для очистки, '+' для закрашивания.";
    print_endline "Нажмите 'show', чтобы увидеть решение, или 'q' для выхода.";
    if check_solution { nonogram with grid } then (
      print_nonogram { nonogram with grid } cursor_x cursor_y;
      print_endline "Поздравляем! Вы решили нонограмму!";
      exit 0;
    );
    let input = read_line () in
    match input with
    | "w" -> play (max 0 (cursor_x - 1)) cursor_y grid
    | "s" -> play (min (size - 1) (cursor_x + 1)) cursor_y grid
    | "a" -> play cursor_x (max 0 (cursor_y - 1)) grid
    | "d" -> play cursor_x (min (size - 1) (cursor_y + 1)) grid
    | "+" ->
        let new_grid = Array.copy grid in
        new_grid.(cursor_x).(cursor_y) <- Some true;
        play cursor_x cursor_y new_grid
    | "-" ->
        let new_grid = Array.copy grid in
        new_grid.(cursor_x).(cursor_y) <- Some false;
        play cursor_x cursor_y new_grid
    | "show" -> show_solution nonogram; play cursor_x cursor_y grid  (* Показываем решение *)
    | "q" -> print_endline "Выход из игры.";
    | _ -> print_endline "Некорректный ввод. Попробуйте снова."; play cursor_x cursor_y grid
  in
  play 0 0 (Array.copy nonogram.grid)  (* Начинаем с копии исходной сетки *)

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