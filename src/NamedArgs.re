open GenTypeCommon;

type groupedArg =
  | Group(fields)
  | Arg(typ);

/**
 * For convenient processing turns consecutive named arguments into a
 * `NamedArgs` group, and individual non-named arguments into `Arg`s.
 */
let rec groupReversed = (~revCurGroup, ~revResult, labeledTypes) =>
  switch (revCurGroup, labeledTypes) {
  | ([], [(Nolabel, typ), ...tl]) =>
    groupReversed(~revCurGroup=[], ~revResult=[Arg(typ), ...revResult], tl)
  /* Add it to the current group, not result. */
  | (_, [(OptLabel(name), typ), ...tl]) =>
    groupReversed(
      ~revCurGroup=[(name, NonMandatory, typ), ...revCurGroup],
      ~revResult,
      tl,
    )
  | (_, [(Label(name), typ), ...tl]) =>
    groupReversed(
      ~revCurGroup=[(name, Mandatory, typ), ...revCurGroup],
      ~revResult,
      tl,
    )
  | ([], []) => revResult
  | ([_grpHd, ..._grpTl], [] as _tl)
  /* Just form the group, and recurse ignoring the (None, t) in that case.
   * it will be handled in recursion. */
  | ([_grpHd, ..._grpTl], [(Nolabel, _), ..._tl]) =>
    groupReversed(
      ~revCurGroup=[],
      ~revResult=[Group(revCurGroup), ...revResult],
      labeledTypes,
    )
  };

/**
 * Special reverse that not only reverses the entire list but also the order of
 * items in the NamedArgs grouping.
 */
let rec reverse = (~soFar=[], lst) =>
  switch (lst) {
  | [] => soFar
  | [Arg(typ), ...tl] => reverse(~soFar=[typ, ...soFar], tl)
  | [Group(fields), ...tl] =>
    reverse(~soFar=[Object(List.rev(fields)), ...soFar], tl)
  };

let group = labeledTypes =>
  labeledTypes |> groupReversed(~revCurGroup=[], ~revResult=[]) |> reverse;