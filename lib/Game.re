type player =
  | PlayerOne
  | PlayerTwo;

type point =
  | Love
  | Fifteen
  | Thirty;

type pointsData = {
  playerOne: point,
  playerTwo: point
};

/* One of the players have forty points. */
type fortyData = {
  player: player, /* The player who has forty points */
  otherPlayerPoint: point
};
/* ex : playerOne has 40 point : 
let fd : fortyData = {player: PlayerOne, otherPlayerPoint: Love};*/

type score =
| Points(pointsData) /* Score pour les deux joueurs < 40 */
/* let startScore : score = Points({playerOne: Love, playerTwo: Love}); */
| Forty(fortyData) /* Score si un joueur atteint 40 */
/* let anotherScore : score = Forty({player: PlayerTwo, otherPlayerPoint:Thirty }); */
| Deuce /* Score si les 2 joueurs ont 40 */
| Advantage(player) /* player a l'avantage (les 2 à 40, mais player a mis 1 pt)  */
| Game(player); /* player a gg la manche */

let scoreWhenDeuce: player => score = winner => Advantage(winner);

let scoreWhenAdvantage: (player, player) => score = 
  (advantagedPlayer, winner) => advantagedPlayer == winner ? Game(winner) : Deuce;

let other = player =>
  switch player {
  | PlayerOne => PlayerTwo
  | PlayerTwo => PlayerOne
  };

let incrementPoint: point => option(point) =
  point =>
    switch point {
    | Love => Some(Fifteen)
    | Fifteen => Some(Thirty)
    | Thirty => None
    };

let scoreWhenForty = (current, winner) =>
  current.player == winner ?
    Game(winner) :
    (
      switch (incrementPoint(current.otherPlayerPoint)) {
      | Some(p) => Forty({...current, otherPlayerPoint: p}) /* si j'ai qqch != None (< 30) alors j'augmente les pts */
      | None => Deuce /* ça veut dire que je suis à 40. Donc deuce */
      }  string_of_point(score.PlayerOne)

    );  | Points(p) => switch(p) {
      | PlayerOne => string_of_player(p) ++ string_of_point(p.PlayerOne)
      | PlayerTwo => string_of_player(p) ++ string_of_point(p.PlayerTwo);
    };
  };
  
let pointFor = (player, current) =>
  switch player {
  | PlayerOne => current.playerOne
  | PlayerTwo => current.playerTwo
  };
  
let scoreWhenPoints = (current, winner) =>
  switch (current |> pointFor(winner) |> incrementPoint) {
  | Some(np) => Points(pointTo(winner, np, current))
  | None =>
    Forty({
      player: winner,
      otherPlayerPoint: current |> pointFor(other(winner))
    })
  };

let scoreWhenGame = winner => Game(winner);

let score = (current, winner) =>
  switch current {
  | Points(p) => scoreWhenPoints(p, winner)
  | Forty(f) => scoreWhenForty(f, winner)
  | Deuce => scoreWhenDeuce(winner)
  | Advantage(a) => scoreWhenAdvantage(a, winner)
  | Game(g) => scoreWhenGame(g)
  };


let newGame = Points({playerOne: Love, playerTwo: Love});

let string_of_player = (player) => switch (player) {
| PlayerOne => "player one"
| PlayerTwo => "player two";
}
let string_of_point = (point) => switch (point) {
| Love => "O"
| Fifteen => "15"
| Thirty => "30"
};

let string_of_score = (score) => switch (score) {
  | Points(score) => "player one : " ++ string_of_point(score.playerOne) ++ " | player two : "++ string_of_point(score.playerTwo)
  | Forty(score) => switch(score.player) {
    | PlayerOne => string_of_player(score.player) ++ " : 40 " ++ " | player two : " ++ string_of_point(score.otherPlayerPoint)
    | PlayerTwo => "player one : " ++ string_of_point(score.otherPlayerPoint) ++ " | "++string_of_player(score.player) ++ " : 40"
  }
  | Deuce => "player one : 40 | player two : 40"
  | Advantage(score) => string_of_player(score) ++ " : avantage"
  | Game(score) => string_of_player(score) ++ " remporte la manche"
};

/**
 * string_of_score(Points({
    playerOne: Love,
    playerTwo: Fifteen
  }));
  - : string = "Player one : 0 | Player two : 15"

  string_of_score(Forty({
    player: PlayerTwo,
    otherPlayerPoint: Fifteen
  }));
  - : string = "player one : 15 | player two : 40"

  string_of_score(Deuce);
  - : string = "player one : 40 | player two : 40"

  string_of_score(Advantage(PlayerTwo));
  - : string = "player two : avantage"

  string_of_score(Game(PlayerTwo));
  - : string = "player two remporte la manche"
 */
