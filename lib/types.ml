type video = {
  title : string;
  ascii : string;
  genre : string;
}

type interaction = {
  video : video;
  (* watchtime : float; *)
  (* Add both watchtime and time before liking? *)
  mutable liked : bool;
}

type user = {
  name : string;
  mutable vid_history : interaction list;
  mutable genre_counts : (string, int) Hashtbl.t
}
