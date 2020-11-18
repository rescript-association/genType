open SharedTypes;

open Infix;
let showConstructor = ({cname: {txt}, args, res}) => {
  txt
  ++ (
    args == []
      ? ""
      : "("
        ++ String.concat(
             ", ",
             args |> List.map(((typ, _)) => typ |> Shared.typeToString),
           )
        ++ ")"
  )
  ++ (res |?>> (typ => "\n" ++ (typ |> Shared.typeToString)) |? "");
};

let rec pathOfModuleOpen = items =>
  switch (items) {
  | [] => Tip("place holder")
  | [one, ...rest] => Nested(one, pathOfModuleOpen(rest))
  };

/* TODO local opens */
let resolveOpens = (~env, ~previous, opens, ~getModule) =>
  List.fold_left(
    (previous, path) => {
      /** Finding an open, first trying to find it in previoulsly resolved opens */
      let rec loop = prev =>
        switch (prev) {
        | [] =>
          switch (path) {
          | Tip(_) => previous
          | Nested(name, path) =>
            switch (getModule(name)) {
            | None =>
              Log.log("Could not get module " ++ name);
              previous; /* TODO warn? */
            | Some(file) =>
              switch (
                Query.resolvePath(
                  ~env=Query.fileEnv(file),
                  ~getModule,
                  ~path,
                )
              ) {
              | None =>
                Log.log("Could not resolve in " ++ name);
                previous;
              | Some((env, _placeholder)) => previous @ [env]
              }
            }
          }
        | [env, ...rest] =>
          switch (Query.resolvePath(~env, ~getModule, ~path)) {
          | None => loop(rest)
          | Some((env, _placeholder)) => previous @ [env]
          }
        };
      Log.log("resolving open " ++ pathToString(path));
      switch (Query.resolvePath(~env, ~getModule, ~path)) {
      | None =>
        Log.log("Not local");
        loop(previous);
      | Some((env, _)) =>
        Log.log("Was local");
        previous @ [env];
      };
    },
    /* loop(previous) */
    previous,
    opens,
  );

let completionForDeclareds = (~pos, declareds, prefix, transformContents) =>
  /* Log.log("complete for declares " ++ prefix); */
  Hashtbl.fold(
    (_stamp, declared, results) =>
      if (Utils.startsWith(declared.name.txt, prefix)
          && Utils.locationContainsFuzzy(declared.scopeLoc, pos)) {
        [{...declared, item: transformContents(declared.item)}, ...results];
      } else {
        /* Log.log("Nope doesn't count " ++ Utils.showLocation(declared.scopeLoc) ++ " " ++ m); */
        results;
      },
    declareds,
    [],
  );

let completionForExporteds =
    (
      exporteds,
      stamps: Hashtbl.t(int, SharedTypes.declared('a)),
      prefix,
      transformContents,
    ) =>
  Hashtbl.fold(
    (name, stamp, results) =>
      /* Log.log("checking exported: " ++ name); */
      if (Utils.startsWith(name, prefix)) {
        let declared = Hashtbl.find(stamps, stamp);
        [{...declared, item: transformContents(declared.item)}, ...results];
      } else {
        results;
      },
    exporteds,
    [],
  );

let completionForConstructors =
    (
      exportedTypes,
      stamps: Hashtbl.t(int, SharedTypes.declared(SharedTypes.Type.t)),
      prefix,
    ) => {
  Hashtbl.fold(
    (_name, stamp, results) => {
      let t = Hashtbl.find(stamps, stamp);
      switch (t.item.kind) {
      | SharedTypes.Type.Variant(constructors) =>
        {
          constructors
          |> List.filter(c => Utils.startsWith(c.cname.txt, prefix))
          |> List.map(c => (c, t));
        }
        @ results
      | _ => results
      };
    },
    exportedTypes,
    [],
  );
};

let completionForAttributes =
    (
      exportedTypes,
      stamps: Hashtbl.t(int, SharedTypes.declared(SharedTypes.Type.t)),
      prefix,
    ) =>
  Hashtbl.fold(
    (_name, stamp, results) => {
      let t = Hashtbl.find(stamps, stamp);
      switch (t.item.kind) {
      | Record(attributes) =>
        (
          attributes
          |> List.filter(c => Utils.startsWith(c.aname.txt, prefix))
          |> List.map(c => (c, t))
        )
        @ results
      | _ => results
      };
    },
    exportedTypes,
    [],
  );

let isCapitalized = name =>
  if (name == "") {
    false;
  } else {
    let c = name.[0];
    switch (c) {
    | 'A'..'Z' => true
    | _ => false
    };
  };

/**
The three possibilities

lower.suffix -> `Attribute([lower, lower], suffix)

lower.Upper.suffix -> `AbsAttribute([Upper], suffix)

Upper.lower -> `Normal([Upper], lower)
*/

let rec pathFromTokenParts = items => {
  switch (items) {
  | [] => assert(false)
  | [one] => Tip(one)
  | [one, ...rest] => Nested(one, pathFromTokenParts(rest))
  };
};

let determineCompletion = items => {
  let rec loop = (offset, items) =>
    switch (items) {
    | [] => assert(false)
    | [one] => `Normal(Tip(one))
    | [one, two] when !isCapitalized(one) => `Attribute(([one], two))
    | [one, two] => `Normal(Nested(one, Tip(two)))
    | [one, ...rest] =>
      if (isCapitalized(one)) {
        switch (loop(offset + String.length(one) + 1, rest)) {
        | `Normal(path) => `Normal(Nested(one, path))
        | x => x
        };
      } else {
        switch (loop(offset + String.length(one) + 1, rest)) {
        | `Normal(path) => `AbsAttribute(path)
        | `Attribute(path, suffix) => `Attribute(([one, ...path], suffix))
        | x => x
        };
      }
    };
  loop(0, items);
};

/* Note: This is a hack. It will be wrong some times if you have a local thing
   that overrides an open.

   Maybe the way to fix it is to make note of what things in an open override
   locally defined things...
   */
let getEnvWithOpens =
    (
      ~pos,
      ~env: Query.queryEnv,
      ~getModule,
      ~opens: list(Query.queryEnv),
      path,
    ) =>
  /* let%opt declared = ; */
  /* for ppx, I think I'd like a "if this is nonnull, bail w/ it".
     So the opposite of let%opt - let%bail or something */
  /* Query.resolvePath(~env, ~path, ~getModule) */
  switch (Query.resolveFromStamps(~env, ~path, ~getModule, ~pos)) {
  | Some(x) => Some(x)
  | None =>
    let rec loop = opens =>
      switch (opens) {
      | [env, ...rest] =>
        Log.log("Looking for env in " ++ env.Query.file.uri);
        switch (Query.resolvePath(~env, ~getModule, ~path)) {
        | Some(x) => Some(x)
        | None => loop(rest)
        };
      | [] =>
        switch (path) {
        | Tip(_) => None
        | Nested(top, path) =>
          Log.log("Getting module " ++ top);
          let%opt file = getModule(top);
          Log.log("got it");
          let env = Query.fileEnv(file);
          Query.resolvePath(~env, ~getModule, ~path)
          |> Infix.logIfAbsent("Unable to resolve the path");
        }
      };
    loop(opens);
  };

type k =
  | Module(moduleKind)
  | Value(Types.type_expr)
  | Type(Type.t)
  | Constructor(constructor, declared(Type.t))
  | Attribute(attribute, declared(Type.t))
  | FileModule(string);

let kindToInt = k =>
  switch (k) {
  | Module(_) => 9
  | FileModule(_) => 9
  | Constructor(_, _) => 4
  | Attribute(_, _) => 5
  | Type(_) => 22
  | Value(_) => 12
  };

let detail = (name, contents) =>
  switch (contents) {
  | Type({decl}) => decl |> Shared.declToString(name)
  | Value(typ) => typ |> Shared.typeToString
  | Module(_) => "module"
  | FileModule(_) => "file module"
  | Attribute({typ}, t) =>
    name
    ++ ": "
    ++ (typ |> Shared.typeToString)
    ++ "\n\n"
    ++ (t.item.decl |> Shared.declToString(t.name.txt))
  | Constructor(c, t) =>
    showConstructor(c)
    ++ "\n\n"
    ++ (t.item.decl |> Shared.declToString(t.name.txt))
  };

let localValueCompletions = (~pos, ~env: Query.queryEnv, suffix) => {
  let results = [];
  Log.log("---------------- LOCAL VAL");
  let results =
    if (suffix == "" || isCapitalized(suffix)) {
      results
      @ completionForDeclareds(~pos, env.file.stamps.modules, suffix, m =>
          Module(m)
        )
      @ (
        /* TODO declared thingsz */
        completionForConstructors(
          env.exported.types,
          env.file.stamps.types,
          suffix,
        )
        |> List.map(((c, t)) =>
             {...emptyDeclared(c.cname.txt), item: Constructor(c, t)}
           )
      );
    } else {
      results;
    };

  let results =
    if (suffix == "" || !isCapitalized(suffix)) {
      results
      @ completionForDeclareds(~pos, env.file.stamps.values, suffix, v =>
          Value(v)
        )
      @ completionForDeclareds(~pos, env.file.stamps.types, suffix, t =>
          Type(t)
        )
      @ (
        completionForAttributes(
          env.exported.types,
          env.file.stamps.types,
          suffix,
        )
        |> List.map(((c, t)) =>
             {...emptyDeclared(c.aname.txt), item: Attribute(c, t)}
           )
      );
    } else {
      results;
    };

  results |> List.map(x => (env.file.uri, x));
};

let valueCompletions = (~env: Query.queryEnv, suffix) => {
  Log.log(" - Completing in " ++ env.file.uri);
  let results = [];
  let results =
    if (suffix == "" || isCapitalized(suffix)) {
      // Get rid of lowercase modules (#417)
      env.exported.modules
      |> Hashtbl.filter_map_inplace((name, key) =>
           isCapitalized(name) ? Some(key) : None
         );

      let moduleCompletions =
        completionForExporteds(
          env.exported.modules, env.file.stamps.modules, suffix, m =>
          Module(m)
        );
      /* Log.log(" -- capitalized " ++ string_of_int(Hashtbl.length(env.exported.types)) ++ " exported types"); */
      /* env.exported.types |> Hashtbl.iter((name, _) => Log.log("    > " ++ name)); */
      results
      @ moduleCompletions
      @ (
        /* TODO declared thingsz */
        completionForConstructors(
          env.exported.types,
          env.file.stamps.types,
          suffix,
        )
        |> List.map(((c, t)) =>
             {...emptyDeclared(c.cname.txt), item: Constructor(c, t)}
           )
      );
    } else {
      results;
    };

  let results =
    if (suffix == "" || !isCapitalized(suffix)) {
      Log.log(" -- not capitalized");
      results
      @ completionForExporteds(
          env.exported.values, env.file.stamps.values, suffix, v =>
          Value(v)
        )
      @ completionForExporteds(
          env.exported.types, env.file.stamps.types, suffix, t =>
          Type(t)
        )
      @ (
        completionForAttributes(
          env.exported.types,
          env.file.stamps.types,
          suffix,
        )
        |> List.map(((c, t)) =>
             {...emptyDeclared(c.aname.txt), item: Attribute(c, t)}
           )
      );
    } else {
      results;
    };

  /* Log.log("Getting value completions " ++ env.file.uri);
     Log.log(String.concat(", ", results |. Belt.List.map(x => x.name.txt))); */

  results |> List.map(x => (env.file.uri, x));
};

let attributeCompletions = (~env: Query.queryEnv, ~suffix) => {
  let results = [];
  let results =
    if (suffix == "" || isCapitalized(suffix)) {
      results
      @ completionForExporteds(
          env.exported.modules, env.file.stamps.modules, suffix, m =>
          Module(m)
        );
    } else {
      results;
    };

  let results =
    if (suffix == "" || !isCapitalized(suffix)) {
      results
      @ completionForExporteds(
          env.exported.values, env.file.stamps.values, suffix, v =>
          Value(v)
        )
      /* completionForExporteds(env.exported.types, env.file.stamps.types, suffix, t => Type(t)) @ */
      @ (
        completionForAttributes(
          env.exported.types,
          env.file.stamps.types,
          suffix,
        )
        |> List.map(((c, t)) =>
             {...emptyDeclared(c.aname.txt), item: Attribute(c, t)}
           )
      );
    } else {
      results;
    };

  results |> List.map(x => (env.file.uri, x));
};

/**

TODO filter out things that are defined after the current position

*/

let resolveRawOpens = (~env, ~getModule, ~rawOpens, ~package) => {
  // TODO Stdlib instead of Pervasives
  let packageOpens = ["Pervasives", ...package.TopTypes.opens];
  Log.log("Package opens " ++ String.concat(" ", packageOpens));

  let opens =
    resolveOpens(
      ~env,
      ~previous=
        List.map(Query.fileEnv, packageOpens |> Utils.filterMap(getModule)),
      rawOpens
      |> List.map(Str.split(Str.regexp_string(".")))
      |> List.map(pathOfModuleOpen),
      ~getModule,
    );

  opens;
};

/** This function should live somewhere else */
let findDeclaredValue =
    (
      ~file,
      ~package,
      /* the text that we found e.g. open A.B.C, this is "A.B.C" */
      ~rawOpens,
      ~getModule,
      pos,
      tokenParts,
    ) => {
  let env = Query.fileEnv(file);

  let opens = resolveRawOpens(~env, ~getModule, ~rawOpens, ~package);

  let path = pathFromTokenParts(tokenParts);

  let%opt (env, suffix) =
    getEnvWithOpens(~pos, ~env, ~getModule, ~opens, path);

  let%opt stamp = Hashtbl.find_opt(env.exported.values, suffix);
  Hashtbl.find_opt(env.file.stamps.values, stamp);
};

let get =
    (~full, ~package, ~rawOpens, ~getModule, ~allModules, pos, tokenParts) => {
  Log.log(
    "Opens folkz > "
    ++ string_of_int(List.length(rawOpens))
    ++ " "
    ++ String.concat(" ... ", rawOpens),
  );
  let env = Query.fileEnv(full.file);

  let packageOpens = ["Pervasives", ...package.TopTypes.opens];
  Log.log("Package opens " ++ String.concat(" ", packageOpens));

  let opens = resolveRawOpens(~env, ~getModule, ~rawOpens, ~package);
  Log.log(
    "Opens nows "
    ++ string_of_int(List.length(opens))
    ++ " "
    ++ String.concat(" ", opens |> List.map(e => e.Query.file.uri)),
  );

  switch (tokenParts) {
  | [] => []
  | [suffix] =>
    let locallyDefinedValues = localValueCompletions(~pos, ~env, suffix);
    let alreadyUsedIdentifiers = Hashtbl.create(10);
    let valuesFromOpens =
      opens
      |> List.fold_left(
           (results, env) => {
             let completionsFromThisOpen = valueCompletions(~env, suffix);
             List.filter(
               ((_uri, declared)) =>
                 if (!Hashtbl.mem(alreadyUsedIdentifiers, declared.name.txt)) {
                   Hashtbl.add(
                     alreadyUsedIdentifiers,
                     declared.name.txt,
                     true,
                   );
                   true;
                 } else {
                   false;
                 },
               completionsFromThisOpen,
             )
             @ results;
           },
           [],
         );
    /* TODO complete the namespaced name too */
    let localModuleNames =
      allModules
      |> Utils.filterMap(name => {
           /* Log.log("Checking " ++ name); */
           Utils.startsWith(name, suffix) && !String.contains(name, '-')
             ? Some((
                 "wait for uri",
                 {...emptyDeclared(name), item: FileModule(name)},
               ))
             : None
         });
    locallyDefinedValues @ valuesFromOpens @ localModuleNames;
  | multiple =>
    open Infix;
    Log.log("Completing for " ++ String.concat("<.>", multiple));

    switch (determineCompletion(multiple)) {
    | `Normal(path) =>
      {
        Log.log("normal " ++ pathToString(path));
        let%opt_wrap (env, suffix) =
          getEnvWithOpens(~pos, ~env, ~getModule, ~opens, path);
        Log.log("Got the env");
        valueCompletions(~env, suffix);
      }
      |? []
    | `Attribute(target, suffix) =>
      {
        Log.log("suffix :" ++ suffix);
        switch (target) {
        | [] => None
        | [first, ...rest] =>
          Log.log("-------------- Looking for " ++ first);
          let%opt declared =
            Query.findInScope(pos, first, env.file.stamps.values);
          Log.log("Found it! " ++ declared.name.txt);
          let%opt path = declared.item |> Shared.digConstructor;
          let%opt (env, typ) = Hover.digConstructor(~env, ~getModule, path);
          let%opt (env, typ) =
            rest
            |> List.fold_left(
                 (current, name) => {
                   let%opt (env, typ) = current;
                   switch (typ.item.SharedTypes.Type.kind) {
                   | Record(attributes) =>
                     let%opt attr =
                       attributes |> List.find_opt(a => a.aname.txt == name);
                     Log.log("Found attr " ++ name);
                     let%opt path = attr.typ |> Shared.digConstructor;
                     Hover.digConstructor(~env, ~getModule, path);
                   | _ => None
                   };
                 },
                 Some((env, typ)),
               );
          switch (typ.item.kind) {
          | Record(attributes) =>
            Some(
              attributes
              |> Utils.filterMap(a =>
                   if (Utils.startsWith(a.aname.txt, suffix)) {
                     Some((
                       env.file.uri,
                       {
                         ...emptyDeclared(a.aname.txt),
                         item: Attribute(a, typ),
                       },
                     ));
                   } else {
                     None;
                   }
                 ),
            )
          | _ => None
          };
        };
      }
      |? []
    | `AbsAttribute(path) =>
      {
        let%opt_wrap (env, suffix) =
          getEnvWithOpens(~pos, ~env, ~getModule, ~opens, path);

        attributeCompletions(~env, ~suffix)
        @ List.concat(
            opens |> List.map(env => attributeCompletions(~env, ~suffix)),
          );
      }
      |? []
    };
  };
};
