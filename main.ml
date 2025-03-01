(* Тип для представления нонограммы *)
type nonogram = {
  rows : int list list;  (* Подсказки для строк *)
  cols : int list list;  (* Подсказки для столбцов *)
  grid : bool option array array;  (* Сетка: None - неизвестно, Some true - закрашено, Some false - пусто *)
  solution : bool array array;  (* Решение для проверки *)
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
    solution;
  }

(* Вывод сетки с подсказками *)
let print_nonogram nonogram =
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
    Array.iter (fun cell ->
      match cell with
      | None -> print_string " . "
      | Some true -> print_string " # "
      | Some false -> print_string " x "
    ) row;
    print_newline ()
  ) nonogram.grid

(* Проверка решения *)
let check_solution nonogram =
  Array.for_all2 (fun row sol_row ->
    Array.for_all2 (fun cell sol_cell ->
      match cell with
      | Some true -> sol_cell
      | Some false -> not sol_cell
      | None -> false
    ) row sol_row
  ) nonogram.grid nonogram.solution

(* Очистка всей таблицы *)
let clear_grid nonogram =
  Array.iteri (fun i row ->
    Array.iteri (fun j _ -> nonogram.grid.(i).(j) <- None) row
  ) nonogram.grid

(* Показать решение *)
let show_solution nonogram =
  Array.iteri (fun i row ->
    Array.iteri (fun j _ ->
      nonogram.grid.(i).(j) <- Some nonogram.solution.(i).(j)
    ) row
  ) nonogram.grid

(* Основной цикл игры *)
let play_nonogram nonogram =
  let rec play () =
    print_nonogram nonogram;
    print_string "Введите команду (например, '2 3 fill' или '1 2 clear'): ";
    let input = read_line () in
    match String.split_on_char ' ' input with
    | [x; y; "fill"] ->
      let x = int_of_string x - 1 in
      let y = int_of_string y - 1 in
      nonogram.grid.(x).(y) <- Some true;
      if check_solution nonogram then (
        print_nonogram nonogram;
        print_endline "Поздравляем! Вы решили нонограмму!"
      ) else play ()
    | [x; y; "clear"] ->
      let x = int_of_string x - 1 in
      let y = int_of_string y - 1 in
      nonogram.grid.(x).(y) <- Some false;
      play ()
    | ["clear"; "all"] ->
       clear_grid nonogram;
       print_endline "Таблица очищена.";
       play ()
    | ["give"; "up"] ->
       show_solution nonogram;
       print_nonogram nonogram;

    | _ ->
      print_endline "Некорректный ввод. Попробуйте снова.";
      play ()
  in
  play ()

(* Запуск игры *)
let () =
  Random.self_init ();
  let size = 5 in  (* Размер нонограммы *)
  let nonogram = generate_nonogram size in
  print_endline "Добро пожаловать в игру Нонограмма!";
  print_endline "Используйте команды 'x y fill' для закрашивания клетки и 'x y clear' для очистки.";
  play_nonogram nonogram