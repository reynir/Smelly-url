exception Not_found
exception Invalid_scheme
exception Invalid_schemepart

fun mem (x, xs) = List.exists (fn a => a = x) xs

fun index (s, c) =
    let val cs = explode s in
        let fun visit (idx, []) = raise Not_found
              | visit (idx, h :: t) =
                if h = c
                then idx
                else visit (idx + 1, t)
        in visit (0, cs)
        end
    end

(* ******************************** *)

fun split_url url =
    let val colon_idx = index (url, #":")
        val scheme = String.extract (url, 0, SOME colon_idx)
        val schemepart = String.extract (url, colon_idx + 1, NONE) in
        (scheme, schemepart)
    end

fun check_scheme scheme =
    if List.all
           (fn c => Char.isAlphaNum c orelse
                    c = #"+" orelse c = #"." orelse c = #"-")
       (explode scheme)
    then ()
    else raise Invalid_scheme

fun check_schemepart_generic schemepart =
    let val unsafe = [#"<", #">", #"\"", #"#", #"%",
                      #"{", #"}", #"|", #"\\", #"^",
                      #"~", #"[", #"]", #"`"] in
        if List.exists
               (fn c => Char.isSpace c orelse mem (c, unsafe))
               (explode schemepart)
        then raise Invalid_schemepart
        else ()
    end

fun unquote quoted =
    let fun unhex (c1, c2) =
            if not (Char.isHexDigit c1 andalso
                    Char.isHexDigit c2)
            then raise Invalid_schemepart
            else let val n = 16 * (Char.ord (Char.toUpper c1) - Char.ord #"0") +
                             Char.ord (Char.toUpper c2) - Char.ord #"0" in
                     Char.chr n
                 end
        and init (c :: cs) =
            if c = #"%"
            then percent cs
            else c :: init cs
          | init [] = []
        and percent (c1 :: c2 :: cs) =
            unhex (c1, c2) :: init cs
          | percent _ = raise Invalid_schemepart in
        implode (init (explode quoted))
    end

(* ******************************** *)

type port = int
type hostname = string
type search = string
type hsegment = string
type hpath = hsegment list
type hostport = hostname * port option

datatype urlinfo
  = HTTP of hostport * hpath * search

fun is_safe c = 
    mem (c, [#"$", #"-", #"_", #".", #"+"])

fun is_extra c =
    mem (c, [#"!", #"*", #"'", #"(", #")", #","])

fun is_national c =
    mem (c, [#"{", #"}", #"|", #"\\", #"^", #"~", #"[", #"]", #"`"])

fun is_punctuation c =
    mem (c, [#"<", #">", #"#", #"%", #"\""])

fun is_unreserved c =
    Char.isAlphaNum c orelse is_safe c orelse is_extra c

fun parse_port fragment =
    let val fragment' = Substring.full fragment
        val (digits, rest) = Substring.splitl Char.isDigit fragment' in
        (Int.fromString (Substring.string digits), (Substring.string rest))
    end
    handle Overflow => raise Invalid_schemepart

fun parse_hostport fragment =
    let val colon_idx = index (fragment, #":")
        val hostname = String.extract (fragment, 0, SOME colon_idx)
        val (port, rest) =
            parse_port (String.extract (fragment, colon_idx + 1, NONE)) in
        ((hostname, port), rest)
    end
    handle Not_found =>
           let val fragment' = Substring.full fragment
               val (hostname, rest) =
                   Substring.splitl (fn c => c <> #"/" andalso c <> #"?") fragment'
           in ((Substring.string hostname, NONE), Substring.string rest)
           end

fun check_allowed_char c =
    if is_unreserved c orelse
       c = #";" orelse
       c = #":" orelse
       c = #"@" orelse
       c = #"&" orelse
       c = #"="
    then ()
    else raise Invalid_schemepart

fun parse_search fragment =
    let fun check (c :: cs) =
            if c = #"%"
            then percent cs
            else (check_allowed_char c; check cs)
          | check [] = ()
        and percent (c1 :: c2 :: cs) =
            if not (Char.isHexDigit c1 andalso Char.isHexDigit c2)
            then raise Invalid_schemepart
            else check cs
          | percent _ = raise Invalid_schemepart
    in check (explode fragment); fragment
    end

fun parse_hsegment fragment =
    String.fields (fn c => c = #"/") fragment

fun parse_hpath fragment =
    let val question_idx = index (fragment, #"?")
        val hpath = String.extract (fragment, 0, SOME question_idx)
        val rest = String.extract (fragment, question_idx, NONE)
    in (parse_hsegment hpath, rest)
    end
    handle Not_found => (parse_hsegment fragment, "")

fun parse_http_schemepart schemepart =
    if not (String.isPrefix "//" schemepart)
    then raise Invalid_schemepart
    else
        let val rest = String.extract (schemepart, 2, NONE)
            val (hostport, rest') = parse_hostport rest
        in
            if String.size rest' > 0 andalso String.sub (rest', 0) = #"/"
            then
                let val (hpath, rest'') =
                        parse_hpath (String.extract (rest', 1, NONE))
                in
                    List.map parse_search hpath;
                    if String.size rest'' > 0 andalso
                       String.sub (rest'', 0) = #"?"
                    then HTTP (hostport, hpath,
                               parse_search (String.extract (rest'', 1, NONE)))
                    else HTTP (hostport, hpath, "")
                end
            else
                HTTP (hostport, [], "")
        end

(* ******************************** *)

