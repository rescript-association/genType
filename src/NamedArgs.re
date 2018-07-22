open GenFlowCommon;

type label =
  | Nolabel
  | Label(string)
  | OptLabel(string);

type groupedArg('t) =
  /* Contains a list of (name, isOptional, 't)  */
  | NamedArgs(list((string, optionalness, 't)))
  | Arg('t);

/**
 * For convenient processing turns consecutive named arguments into a
 * `NamedArgs` group, and individual non-named arguments into `Arg`s.
 */
let rec groupReversed = (revCurGroup, revResult, lst) =>
  switch (revCurGroup, lst) {
  | ([], [(Nolabel, t), ...tl]) =>
    groupReversed([], [Arg(t), ...revResult], tl)
  /* Add it to the current group, not result. */
  | (_, [(OptLabel(name), t), ...tl]) =>
    groupReversed([(name, NonMandatory, t), ...revCurGroup], revResult, tl)
  | (_, [(Label(name), t), ...tl]) =>
    groupReversed([(name, Mandatory, t), ...revCurGroup], revResult, tl)
  | ([], []) => revResult
  | ([grpHd, ...grpTl], [] as _tl)
  /* Just form the group, and recurse ignoring the (None, t) in that case.
   * it will be handled in recursion. */
  | ([grpHd, ...grpTl], [(Nolabel, _), ..._tl]) =>
    groupReversed([], [NamedArgs(revCurGroup), ...revResult], lst)
  };

/**
 * Special reverse that not only reverses the entire list but also the order of
 * items in the NamedArgs grouping.
 */
let rec reverse = (~soFar=[], lst) =>
  switch (lst) {
  | [] => soFar
  | [Arg(t) as hd, ...tl] => reverse(~soFar=[hd, ...soFar], tl)
  | [NamedArgs(namedArgs), ...tl] =>
    reverse(~soFar=[NamedArgs(List.rev(namedArgs)), ...soFar], tl)
  };