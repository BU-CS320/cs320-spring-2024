type user = {
  name : string ;
  id : int ;
  num_likes : int ;
}

let capitalize =
  let up_first i c =
    if i = 0 then
      Char.uppercase_ascii c
    else
      c
    in String.mapi up_first

let fix_usernames (us : user list)  =
  List.map (fun u -> { u with name = capitalize u.name }) us

let popular (us : user list) (cap : int) =
  List.filter (fun u -> u.num_likes > cap) us

let even n = n mod 2 = 0