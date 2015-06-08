module Main (main) where

import Prelude ((<$>), (<*>), (<<<), (+), return, unit, Unit())
import Control.Monad.Eff (Eff())
import DOM (DOM(), Node())
import Signal (foldp, runSignal, sampleOn, Signal())
import Signal.DOM (animationFrame, keyPressed)

import Mario (marioLogic, charSpriteDescriptor, enemySpriteDescriptor, enemyCharacter, makeRandomEnemy, enemyLogic, Character(), SpriteDescriptor(), Direction(Left, Right), Enemy())

type GameState = { mario :: Character, enemies :: [Enemy] }

groundHeight = 40 -- px
leftKeyCode = 37
rightKeyCode = 39
jumpKeyCode = 38
maxEnemies = 3


foreign import updatePositionP """
  function updatePositionP(c) {
    return function() {
      if (c.node.parentNode == null) {
        document.body.insertBefore(c.node, document.body.firstChild);
      }
      c.node.style.left = c.x + "px";
      c.node.style.bottom = c.y + "px";
    };
  }
  """ :: forall eff. Character -> Eff (dom :: DOM | eff) Unit

updatePosition :: Character -> Eff _ Unit
updatePosition = updatePositionP <<< offsetY groundHeight
  where
  offsetY :: Number -> Character -> Character
  offsetY amount c = c { y = c.y + amount }

foreign import updateSpriteP """
  function updateSpriteP(node) {
  return function(className) {
    return function() {
      node.className = className;
    };
  };}
  """ :: forall eff. Node -> SpriteDescriptor -> Eff (dom :: DOM | eff) Unit

updateSprite :: forall eff. Character -> Eff _ Unit
updateSprite c = updateSpriteP c.node (charSpriteDescriptor c)

foreign import data Trace :: !
foreign import trace """
  function trace(x) {
    return function() {
      window.state = x;
      //console.dir(x);
    };
  }
  """ :: forall a eff. a -> forall r. Eff r Unit

foreign import atRandomInterval """
  function atRandomInterval(next) {
  return function(min) {
  return function(max) {
    function t(){ return min + (Math.random() * (max - min)); }
    var a = [];
    setTimeout(function iterate() {
      a = [next()];
      setTimeout(iterate, t());
    }, t());
    return function() {
      var b = a;
      a = [];
      return b;
    };
  };};}
  """ :: forall a eff. Eff (|eff) a -> Number -> Number -> Eff (|eff) [a]

foreign import onDOMContentLoaded """
  function onDOMContentLoaded(action) {
    if (document.readyState === "interactive") {
      action();
    } else {
      document.addEventListener("DOMContentLoaded", action);
    }
    return function() { return {}; };
  }
  """ :: forall a eff. Eff (dom :: DOM | eff) a -> Eff (eff) Unit

foreign import getMarioNode """
  function getMarioNode() { return document.getElementById("mario"); }
  """ :: forall eff. Eff (dom :: DOM | eff) Node

foreign import removeNode """
  function removeNode(node) {
    return function() {
      node.parentNode.removeChild(node);
    };
  }
  """ :: forall eff. Node -> Eff (dom :: DOM | eff) Unit

cleanupOffscreenEnemies :: GameState -> Eff _ GameState
cleanupOffscreenEnemies s = do
  let oobEnemies = filter cond s.enemies
  for oobEnemies \enemy -> removeNode (enemyCharacter enemy).node
  return $ if length oobEnemies == 0 then s else s { enemies = filter (not <<< cond) s.enemies }
    where
      cond enemy =
        let c = enemyCharacter enemy in
        case c.dir of
          Left -> c.x < 50
          Right -> c.x > 500

addEnemy :: Enemy -> GameState -> GameState
addEnemy x s = s { enemies = x : s.enemies }

enemyGenerator :: Eff _ [Enemy]
enemyGenerator = atRandomInterval makeRandomEnemy 10000 10000


initialState :: Node -> GameState
initialState marioNode = {
  mario: {
    node: marioNode,
    x: -50,
    y: 0,
    dx: 3,
    dy: 6,
    dir: Right
  },
  enemies: []
}

gameLogic :: { left :: Boolean, right :: Boolean, jump :: Boolean } -> Eff _ GameState -> Eff _ GameState
gameLogic inputs gameState = do
  gs <- gameState
  gs' <- if length gs.enemies >= maxEnemies then return gs else do
    newEnemies <- enemyGenerator
    return $ foldr addEnemy gs $ take (maxEnemies - length gs.enemies) newEnemies
  let gs'' = gs' { mario = marioLogic inputs gs'.mario, enemies = enemyLogic <$> gs'.enemies }
  return $ cleanupOffscreenEnemies gs''

render :: Eff _ GameState -> Eff _ Unit
render gameState = do
  gs <- gameState
  for gs.enemies \enemy -> do
    let c = enemyCharacter enemy
    updatePosition c
    updateSpriteP c.node (enemySpriteDescriptor enemy)
  updatePosition gs.mario
  updateSprite gs.mario

main :: Eff _ Unit
main = onDOMContentLoaded do
  marioNode <- getMarioNode
  let initialEffState = return (initialState marioNode)
  frames <- animationFrame
  leftInputs <- keyPressed leftKeyCode
  rightInputs <- keyPressed rightKeyCode
  jumpInputs <- keyPressed jumpKeyCode
  let inputs = { left: _, right: _, jump: _ } <$> leftInputs <*> rightInputs <*> jumpInputs
  let game = foldp gameLogic initialEffState (sampleOn frames inputs)
  runSignal (render <$> game)
