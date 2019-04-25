/*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

open Lwt.Infix;

let store =
  Irmin_test.store((module Irmin_pack.Make), (module Irmin.Metadata.None));

let test_file = "test-db-pack";

let config = Irmin_pack.config(~fresh=false, test_file);

let clean = () => {
  let (module S): (module Irmin_test.S) = store;
  let config = Irmin_pack.config(~fresh=true, test_file);
  S.Repo.v(config)
  >>= (
    repo => S.Repo.branches(repo) >>= Lwt_list.iter_p(S.Branch.remove(repo))
  );
};

let init = () => {
  if (Sys.file_exists(test_file)) {
    let cmd = Printf.sprintf("rm -rf %s", test_file);
    let _ = Sys.command(cmd);
    ();
  };
  Lwt.return_unit;
};

let stats = None;

let suite = {Irmin_test.name: "PACK", init, clean, config, store, stats};

module Dict = Irmin_pack.Dict;

let test_dict = (_switch, ()) =>
  Dict.v(~fresh=true, test_file)
  >>= (
    dict =>
      Dict.index(dict, "foo")
      >>= (
        x1 => {
          Alcotest.(check(int))("foo", 0, x1);
          Dict.index(dict, "foo")
          >>= (
            x1 => {
              Alcotest.(check(int))("foo", 0, x1);
              Dict.index(dict, "bar")
              >>= (
                x2 => {
                  Alcotest.(check(int))("bar", 1, x2);
                  Dict.index(dict, "toto")
                  >>= (
                    x3 => {
                      Alcotest.(check(int))("toto", 2, x3);
                      Dict.index(dict, "titiabc")
                      >>= (
                        x4 => {
                          Alcotest.(check(int))("titiabc", 3, x4);
                          Dict.index(dict, "foo")
                          >>= (
                            x1 => {
                              Alcotest.(check(int))("foo", 0, x1);
                              Dict.v(~fresh=false, test_file)
                              >>= (
                                dict2 =>
                                  Dict.index(dict2, "titiabc")
                                  >>= (
                                    x4 => {
                                      Alcotest.(check(int))(
                                        "titiabc",
                                        3,
                                        x4,
                                      );
                                      Dict.find(dict2, x1)
                                      >>= (
                                        v1 => {
                                          Alcotest.(check(option(string)))(
                                            "find x1",
                                            Some("foo"),
                                            v1,
                                          );
                                          Dict.find(dict2, x2)
                                          >>= (
                                            v2 => {
                                              Alcotest.(
                                                check(option(string))
                                              )(
                                                "find x2",
                                                Some("bar"),
                                                v2,
                                              );
                                              Dict.find(dict2, x3)
                                              >>= (
                                                v3 => {
                                                  Alcotest.(
                                                    check(option(string))
                                                  )(
                                                    "find x3",
                                                    Some("toto"),
                                                    v3,
                                                  );
                                                  Lwt.return();
                                                }
                                              );
                                            }
                                          );
                                        }
                                      );
                                    }
                                  )
                              );
                            }
                          );
                        }
                      );
                    }
                  );
                }
              );
            }
          );
        }
      )
  );

module Index = Irmin_pack.Index(Irmin.Hash.SHA1);

let get =
  fun
  | Some(x) => x
  | None => Alcotest.fail("None");

let pp_hash = Irmin.Type.pp(Irmin.Hash.SHA1.t);

let hash = Alcotest.testable(pp_hash, Irmin.Type.equal(Irmin.Hash.SHA1.t));

let test_index = (_switch, ()) =>
  Index.v(~fresh=true, test_file)
  >>= (
    t => {
      let h1 = Irmin.Hash.SHA1.digest("foo");
      let o1 = 42L;
      let h2 = Irmin.Hash.SHA1.digest("bar");
      let o2 = 142L;
      let h3 = Irmin.Hash.SHA1.digest("otoo");
      let o3 = 10098L;
      let h4 = Irmin.Hash.SHA1.digest("sdadsadas");
      let o4 = 8978232L;
      Lwt_list.iter_s(
        ((h, off)) => Index.append(t, h, ~off, ~len=42),
        [(h1, o1), (h2, o2), (h3, o3), (h4, o4)],
      )
      >>= (
        () => {
          let test = t =>
            Index.find(t, h1)
            >|= get
            >>= (
              x1 => {
                Alcotest.(check(int64))("h1", o1, x1.offset);
                Alcotest.(check(int))("h1 id", 0, x1.id);
                Index.find(t, h2)
                >|= get
                >>= (
                  x2 => {
                    Alcotest.(check(int64))("h2", o2, x2.offset);
                    Alcotest.(check(int))("h2 id", 1, x2.id);
                    Alcotest.(check(int))("h2 len", 42, x2.len);
                    Index.find(t, h3)
                    >|= get
                    >>= (
                      x3 => {
                        Alcotest.(check(int64))("h3", o3, x3.offset);
                        Alcotest.(check(int))("h3 id", 2, x3.id);
                        Index.find(t, h4)
                        >|= get
                        >>= (
                          x4 => {
                            Alcotest.(check(int64))("h4", o4, x4.offset);
                            Lwt_list.iteri_s(
                              (i, (h, o)) =>
                                Index.read(t, i)
                                >|= get
                                >|= (
                                  e => {
                                    Alcotest.(check(int))(
                                      "entry id",
                                      i,
                                      e.id,
                                    );
                                    Alcotest.(check(int64))(
                                      "entry off",
                                      o,
                                      e.offset,
                                    );
                                    Alcotest.(check(hash))(
                                      "entry hash",
                                      h,
                                      e.hash,
                                    );
                                  }
                                ),
                              [(h1, o1), (h2, o2), (h3, o3)],
                            );
                          }
                        );
                      }
                    );
                  }
                );
              }
            );

          test(t) >>= (() => Index.v(~fresh=false, test_file) >>= test);
        }
      );
    }
  );

module S = {
  include Irmin.Contents.String;

  type hash = Irmin.Hash.SHA1.t;

  let to_bin = (~dict as _, ~index as _, x) =>
    Lwt.return(Irmin.Type.to_bin_string(t, x));

  let of_bin = (~dict as _, ~index as _, x) =>
    Lwt.return(Irmin.Type.of_bin_string(t, x));
};

module P = Irmin_pack.Pack(Irmin.Hash.SHA1);
module Pack = P.Make(S);

let test_pack = (_switch, ()) =>
  Pack.v(~fresh=true, test_file)
  >>= (
    t => {
      let x1 = "foo";
      let x2 = "bar";
      let x3 = "otoo";
      let x4 = "sdadsadas";
      let h1 = Irmin.Hash.SHA1.digest(x1);
      let h2 = Irmin.Hash.SHA1.digest(x2);
      let h3 = Irmin.Hash.SHA1.digest(x3);
      let h4 = Irmin.Hash.SHA1.digest(x4);
      Lwt_list.iter_s(
        ((k, v)) => Pack.append(t, k, v),
        [(h1, x1), (h2, x2), (h3, x3), (h4, x4)],
      )
      >>= (
        () => {
          let test = t =>
            Pack.find(t, h1)
            >|= get
            >>= (
              y1 => {
                Alcotest.(check(string))("x1", x1, y1);
                Pack.find(t, h3)
                >|= get
                >>= (
                  y3 => {
                    Alcotest.(check(string))("x3", x3, y3);
                    Pack.find(t, h2)
                    >|= get
                    >>= (
                      y2 => {
                        Alcotest.(check(string))("x2", x2, y2);
                        Pack.find(t, h4)
                        >|= get
                        >>= (
                          y4 => {
                            Alcotest.(check(string))("x4", x4, y4);
                            Lwt.return();
                          }
                        );
                      }
                    );
                  }
                );
              }
            );

          test(t) >>= (() => Pack.v(~fresh=false, test_file) >>= test);
        }
      );
    }
  );

module Branch = Irmin_pack.Atomic_write(Irmin.Branch.String, Irmin.Hash.SHA1);

let test_branch = (_switch, ()) => {
  let branches = ["foo", "bar/toto", "titi"];
  let test = t =>
    Lwt_list.iter_s(
      k => Branch.set(t, k, Irmin.Hash.SHA1.digest(k)),
      branches,
    )
    >>= (
      () => {
        let check = h =>
          Branch.find(t, h)
          >|= (
            v =>
              Alcotest.(check(option(hash)))(
                h,
                Some(Irmin.Hash.SHA1.digest(h)),
                v,
              )
          );

        Lwt_list.iter_p(check, branches);
      }
    );

  Branch.v(~fresh=true, test_file)
  >>= test
  >>= (
    () =>
      Branch.v(~fresh=true, test_file)
      >>= test
      >>= (
        () =>
          Branch.v(~fresh=true, test_file)
          >>= test
          >>= (
            () =>
              Branch.v(~fresh=false, test_file)
              >>= (
                t =>
                  test(t)
                  >>= (
                    () => {
                      let x = Irmin.Hash.SHA1.digest("XXX");
                      Branch.set(t, "foo", x)
                      >>= (
                        () =>
                          Branch.v(~fresh=false, test_file)
                          >>= (
                            t =>
                              Branch.find(t, "foo")
                              >>= (
                                v => {
                                  Alcotest.(check(option(hash)))(
                                    "foo",
                                    Some(x),
                                    v,
                                  );
                                  Branch.list(t)
                                  >>= (
                                    br => {
                                      Alcotest.(
                                        check(slist(string, compare))
                                      )(
                                        "branches",
                                        branches,
                                        br,
                                      );
                                      Branch.remove(t, "foo")
                                      >>= (
                                        () =>
                                          Branch.v(~fresh=false, test_file)
                                          >>= (
                                            t =>
                                              Branch.find(t, "foo")
                                              >>= (
                                                v => {
                                                  Alcotest.(
                                                    check(option(hash))
                                                  )(
                                                    "foo none",
                                                    None,
                                                    v,
                                                  );
                                                  Branch.list(t)
                                                  >>= (
                                                    br => {
                                                      Alcotest.(
                                                        check(
                                                          slist(
                                                            string,
                                                            compare,
                                                          ),
                                                        )
                                                      )(
                                                        "branches",
                                                        List.filter(
                                                          (!=)("foo"),
                                                          branches,
                                                        ),
                                                        br,
                                                      );
                                                      Lwt.return();
                                                    }
                                                  );
                                                }
                                              )
                                          )
                                      );
                                    }
                                  );
                                }
                              )
                          )
                      );
                    }
                  )
              )
          )
      )
  );
};

let misc = (
  "misc",
  [
    Alcotest_lwt.test_case("dict", `Quick, test_dict),
    Alcotest_lwt.test_case("index", `Quick, test_index),
    Alcotest_lwt.test_case("pack", `Quick, test_pack),
    Alcotest_lwt.test_case("branch", `Quick, test_branch),
  ],
);
