(***************************************************************************)
(*                                                                         *)
(*   Copyright (c) 2014-2016 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the MIT License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)

                (********   ATTRIBUTES   ********)
module LocSet = Set.Make(struct type t = Lexing.position let compare = compare end)

module LocHash = struct
  include
    Hashtbl.Make(struct
      type t = Lexing.position

      let hash x =
        let s = Filename.basename x.Lexing.pos_fname in
        Hashtbl.hash (x.Lexing.pos_cnum, s)

      let equal x y = x = y
    end)

  let find_set h k = try find h k with Not_found -> LocSet.empty

  let add_set h k v =
    let l = find_set h k in replace h k (LocSet.add v l)

  let merge_set h1 k1 h2 k2 =
    let l1 = find_set h1 k1 in
    let l2 = find_set h2 k2 in
    replace h1 k1 (LocSet.union l1 l2)
end

let abspath : (string, string) Hashtbl.t = Hashtbl.create 256                  (* longest paths known *)


let decs : (Lexing.position, string * string) Hashtbl.t = Hashtbl.create 256                         (* all exported value declarations *)

let incl : (Lexing.position, string * string) Hashtbl.t = Hashtbl.create 256                         (* all exported value declarations *)

let references : LocSet.t LocHash.t  = LocHash.create 256      (* all value references *)

let fields : (string, Lexing.position) Hashtbl.t = Hashtbl.create 256      (* link from fields (record/variant) paths and locations *)

let style : (string * Lexing.position * string) list ref = ref []                        (* patterns of type unit which are not () *)
let last_loc = ref Lexing.dummy_pos                                  (* helper to diagnose occurrences of Location.none in the typedtree *)
let current_src = ref ""
let mods : string list ref = ref []                                                 (* module path *)


let depth = ref (-1)

let declarations = ref true

let _none = "_none_"
let _obj = "*obj*"
let _include = "*include*"
let _variant = ": variant :"


                (********   HELPERS   ********)

let unit fn = try Filename.chop_extension (Filename.basename fn) with Invalid_argument _ -> fn


let is_ghost loc =
  loc.Lexing.pos_lnum <= 0 || loc.Lexing.pos_cnum - loc.Lexing.pos_bol < 0
  || loc.Lexing.pos_fname = _none || loc.Lexing.pos_fname = ""


let check_underscore name = not !DeadFlag.underscore || name.[0] <> '_'


let hashtbl_find_list hashtbl key = Hashtbl.find_all hashtbl key

let hashtbl_add_to_list hashtbl key elt = Hashtbl.add hashtbl key elt

let hashtbl_add_unique_to_list hashtbl key elt =
  if not (List.mem elt (hashtbl_find_list hashtbl key)) then
    Hashtbl.add hashtbl key elt

let rec hashtbl_remove_list hashtbl key =
  if Hashtbl.mem hashtbl key then begin
    Hashtbl.remove hashtbl key;
    hashtbl_remove_list hashtbl key
  end

let hashtbl_replace_list hashtbl key l =
  hashtbl_remove_list hashtbl key;
  List.iter (fun elt -> hashtbl_add_to_list hashtbl key elt) l

let hashtbl_merge_unique_list tbl1 key1 tbl2 key2 =
  List.iter (fun elt -> hashtbl_add_unique_to_list tbl1 key1 elt) (hashtbl_find_list tbl2 key2)

let find_path fn ?(sep = '/') l = List.find
  (fun path ->
    let lp = String.length path and lf = String.length fn in
    (lp > lf && path.[lp - lf - 1] = sep || lp = lf) && String.sub path (lp - lf) lf = fn)
  l

let find_abspath fn =
  find_path fn (hashtbl_find_list abspath (unit fn))




let exported (flag : DeadFlag.basic ref) loc =
  let fn = loc.Lexing.pos_fname in
  !flag.DeadFlag.print
  && LocHash.find_set references loc
     |> LocSet.cardinal <= !flag.DeadFlag.threshold
  && (flag == DeadFlag.typ
    || !DeadFlag.internal
    || fn.[String.length fn - 1] = 'i'
    || unit !current_src <> unit fn
    || try not (Sys.file_exists (find_abspath fn ^ "i")) with Not_found -> true)


(* Section printer:
 * section:     `.> SECTION: '
 *              `==========='
 * subsection:  `.>->  SUBSECTION: '
 *              `~~~~~~~~~~~~~~~~~' *)
let section ?(sub = false) title =
  Printf.printf "%s %s:\n%s\n"
    (if sub then ".>-> " else ".>")
    title
    (String.make ((if sub then 5 else 2) + String.length title + 1) (if sub then '~' else '='))


(* End of section *)
let separator () =
  print_endline "Nothing else to report in this section";
  Printf.printf "%s\n\n\n" (String.make 80 '-')


(* Location printer: `filename:line: ' *)
let prloc ?(call_site = false) ?fn (loc : Lexing.position) =
  let file = loc.Lexing.pos_fname in
  let line = loc.Lexing.pos_lnum in
  let col = loc.Lexing.pos_cnum - loc.Lexing.pos_bol in
  begin match fn with
  | Some s ->
      (* print_string (Filename.dirname s ^ "/" ^ file) *)
      (* s should contain the whole path (thus be sufficient) *)
      print_string s
  | _ -> match find_abspath file with
    | s -> print_string s
    | exception Not_found ->
                Printf.printf "!!UNKNOWN<%s>!!%!" file
  end;
  print_char ':';
  print_int line;
  if call_site then begin
    print_char ':';
    print_int col
  end
  else
    print_string ": "



                (********   TYPES   *********)


type opt_arg =
  {
    mutable with_val: Lexing.position list;
    mutable without_val: Lexing.position list;
  }


let opt_args : (Lexing.position * string * bool * Lexing.position) list ref = ref []



                (********   NODE MANIPULATION   ********)

module VdNode = struct

  type t = (string list * Lexing.position option)

  let vd_nodes = LocHash.create 256

  let parents = LocHash.create 256


  (* Get or create a vd_node corresponding to the location *)
  let get loc =
    assert (not (is_ghost loc));
    try (LocHash.find vd_nodes loc)
    with Not_found ->
      let r = ([], None) in
      LocHash.add vd_nodes loc r;
      r

  let get_opts loc =
    fst (get loc)

  let get_next loc =
    snd (get loc)

  let update loc ((_, loc2) as node) =
    let _, loc1 = get loc in
    begin match loc1 with
    | Some loc1 ->
        LocHash.find_set parents loc1
        |> LocSet.filter (( <> ) loc)
        |> LocHash.replace parents loc1
    | None -> ()
    end;
    begin match loc2 with
    | Some loc2 -> LocHash.add_set parents loc2 loc
    | None -> ()
    end;
    LocHash.replace vd_nodes loc node

  let is_end loc =
    get_next loc = None

  let seen loc =
    try ignore (find_abspath loc.Lexing.pos_fname); true
    with Not_found -> false


  let func loc =
    let met = LocHash.create 8 in
    let rec loop loc =
      LocHash.replace met loc ();
      match get loc with
      | [], Some loc
      when not (LocHash.mem met loc) && seen loc ->
          loop loc
      | _ -> loc
    in loop loc


  (* Locations l1 and l2 are part of a binding from one to another *)
  let merge_locs ?(force = false) loc1 loc2 =
    if not (is_ghost loc1 || is_ghost loc2) then
      let loc2 = func loc2 in
      if force || not (is_end loc2) || get_opts loc2 <> [] || not (seen loc2) then
        let repr loc =
          let met = Hashtbl.create 8 in
          let rec loop loc =
            Hashtbl.add met loc ();
            match get loc with
            | _, Some loc when not (Hashtbl.mem met loc) -> loop loc
            | _ -> loc
          in loop loc
        in
        let loc1 = repr loc1 in
        if loc1 <> loc2 then begin
          let opts, _ = get loc1 in
          update loc1 (opts, Some loc2);
        end


  let find loc lab occur =
    let met = LocHash.create 8 in
    let rec loop loc lab occur =
      let count =
        if is_end loc then 0
        else List.filter (( = ) lab) (get_opts loc) |> List.length
      in
      if is_end loc || LocHash.mem met loc || count >= !occur then loc
      else begin
        occur := !occur - count;
        LocHash.replace met loc ();
        match get_next loc with
        | Some next -> loop (func next) lab occur
        | None ->
            let loc =
              loc.Lexing.pos_fname ^ ":"
              ^ (string_of_int loc.Lexing.pos_lnum)
            in
              failwith (loc ^ ": optional argument `" ^ lab ^ "' unlinked")
      end
    in loop (func loc) lab occur

  let eom () =

    let sons =
      LocHash.fold (fun loc _ acc -> loc :: acc) parents []
      |> List.sort_uniq compare
    in

    let delete loc =
      if seen loc then
        let met = Hashtbl.create 64 in
        let rec loop loc =
          if not (Hashtbl.mem met loc) then begin
            Hashtbl.add met loc ();
            LocHash.find_set parents loc
            |> LocSet.iter loop;
            let pts =
              LocHash.find_set parents loc
              |> LocSet.filter (LocHash.mem vd_nodes) in
            if LocSet.is_empty pts then begin
              if LocHash.mem parents loc then
                LocHash.remove parents loc;
              LocHash.remove vd_nodes loc;
            end
          end
        in loop loc
    in
    List.iter delete sons;

    let delete loc =
      let met = LocHash.create 64 in
      let rec loop worklist loc_list =
        if not (LocSet.is_empty worklist) then
          let loc = LocSet.choose worklist in
          let wl = LocSet.remove loc worklist in
           if unit loc.Lexing.pos_fname <> unit !current_src then
            List.iter (LocHash.remove parents) loc_list
           else begin
            LocHash.replace met loc ();
            let my_parents = LocHash.find_set parents loc in
            let my_parents =
              LocSet.filter (fun l -> not (LocHash.mem met l)) my_parents
            in
            let wl = LocSet.union my_parents wl in
            loop wl (loc :: loc_list)
           end
      in loop (LocSet.singleton loc) []
    in
    List.iter delete sons


end

                (********   PROCESSING  ********)

let export ?(sep = ".") path u stock id loc =
  let value =
    String.concat "." (List.rev_map Ident.name path)
    ^ sep
    ^ id.Ident.name
  in
  (* a .cmi file can contain locations from other files.
    For instance:
        module M : Set.S with type elt = int
    will create value definitions whose location is in set.mli
  *)
  if not loc.Location.loc_ghost
  && (u = unit loc.Location.loc_start.Lexing.pos_fname || u == _include)
  && check_underscore id.Ident.name then
    hashtbl_add_to_list stock loc.Location.loc_start (!current_src, value)



                (**** REPORTING ****)

(* Absolute path *)
let abs loc = match find_abspath loc.Lexing.pos_fname with
  | s -> s
  | exception Not_found -> loc.Lexing.pos_fname


(* Check directory change *)
let dir first =
  let prev = ref @@ Filename.dirname first
  in fun s -> let s = Filename.dirname s in
    !prev <> s && (prev := s; true)


(* Faster than 'List.length l = len' when len < List.length l; same speed otherwise*)
let rec check_length len = function
  | [] -> len = 0
  | _::l when len > 0 -> check_length (len - 1) l
  | _ -> false


(* Print call site *)
let pretty_print_call () = let ghost = ref false in function
  | loc when not (is_ghost loc) ->
      prloc ~call_site:true loc |> print_newline
  | _ when !ghost -> ()
  | _ ->          (* first ghost met *)
      print_endline "~ ghost ~";
      ghost := true


let percent (opt : DeadFlag.opt) base =
  let open DeadFlag in
  1. -. (float_of_int base) *. (1. -. opt.threshold.percentage) /. 10.


(* Base pattern for reports *)
let report s ~(opt: DeadFlag.opt) ?(extra = "Called") l continue nb_call pretty_print reporter =
  if nb_call = 0 || l <> [] then begin
    section ~sub:(nb_call <> 0)
    @@ (if nb_call = 0 then s
        else if DeadFlag.(opt.threshold.optional) = `Both || extra = "Called"
        then
          Printf.sprintf "%s: %s %d time(s)" s extra nb_call
        else Printf.sprintf "%s: at least %3.2f%% of the time" s (100. *. percent opt nb_call));
    List.iter pretty_print l;
    if continue nb_call then
      (if l <> [] then print_endline "--------" else ()) |> print_newline |> print_newline
  end;
  if continue nb_call then reporter (nb_call + 1)
  else (print_newline () |> separator)


let report_basic ?folder decs title (flag:DeadFlag.basic) =
  let folder = match folder with
    | Some folder -> folder
    | None -> fun nb_call -> fun loc (fn, path) acc ->
        let rec cut_main s pos =
          if pos = String.length s then s
          else if s.[pos] = '.' then String.sub s (pos + 1) (String.length s - pos - 1)
          else cut_main s (pos + 1)
        in
        let test elt =
          let set = LocHash.find_set references elt in
          if LocSet.cardinal set = nb_call then begin
              let l = LocSet.elements set in
              Some ((fn, cut_main path 0, loc, l) :: acc)
            end
          else None
        in match test loc with
          | exception Not_found when nb_call = 0 ->
                (fn, cut_main path 0, loc, []) :: acc
          | exception Not_found -> acc
          | None -> acc
          | Some l -> l
  in
  let rec reportn nb_call =
    let l =
     Hashtbl.fold (folder nb_call) decs []
      |> List.fast_sort (fun (fn1, path1, loc1, _) (fn2, path2, loc2, _) ->
          compare (fn1, loc1, path1) (fn2, loc2, path2))
    in

    let change =
      let (fn, _, _, _) = try List.hd l with _ -> (_none, "", !last_loc, []) in
      dir fn
    in
    let pretty_print = fun (fn, path, loc, call_sites) ->
      if change fn then print_newline ();
      prloc ~fn loc;
      print_string path;
      if call_sites <> [] && flag.DeadFlag.call_sites then
        print_string "    Call sites:";
      print_newline ();
      if flag.DeadFlag.call_sites then begin
        List.fast_sort compare call_sites
        |> List.iter (pretty_print_call ());
        if nb_call <> 0 then print_newline ()
      end
    in

    let continue nb_call = nb_call < flag.DeadFlag.threshold in
    let s =
      if nb_call = 0 then title
      else "ALMOST " ^ title
    in
    report s ~opt:(!DeadFlag.opta) l continue nb_call pretty_print reportn

  in reportn 0

                (********   LEXIFI SPECIALS ********)

module DeadLexiFi = struct
(*   .^.  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  .^.   *)
(*  / ! \  DO NOT DELETE UNLESS YOU CAN COMPILE WITH `make lexifi' AND YOU KNOW WHAT YOU ARE DOING  / ! \  *)
(* /_____\   /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\  -  /!\   /_____\ *)

  (* The following hooks are pointing LexiFi's extensions if compiled with `make lexifi'.
   * They stay as following otherwise *)

  let sig_value : (Types.value_description -> unit) ref =
    ref (fun _ -> ())

  let export_type : (Lexing.position -> string -> unit) ref =
    ref (fun _ _ -> ())

  let type_ext : (Typedtree.core_type -> unit) ref =
    ref (fun _ -> ())

  let type_decl : (Typedtree.type_declaration -> unit) ref =
    ref (fun _ -> ())

  let tstr_type : (Typedtree.type_declaration -> string -> unit) ref =
    ref (fun _ _ -> ())

  let ttype_of : (Typedtree.expression -> unit) ref =
    ref (fun _ -> ())

  let prepare_report : ((Lexing.position, string * string) Hashtbl.t -> unit) ref =
    ref (fun _ -> ())
end
