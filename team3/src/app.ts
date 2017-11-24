import 'p2';
import 'pixi';
import 'phaser';

import {BootState} from './states/boot'
import {SplashState} from './states/splash'
import {GameState} from './states/game'

class Game extends Phaser.Game {

  constructor () {
    super(1024, 576, Phaser.AUTO, 'content', null)

    this.state.add('Boot', BootState, false)
    this.state.add('Splash', SplashState, false)
    this.state.add('Game', GameState, false)

    this.state.start('Boot')
  }
}

window.onload = () => {
  const game = new Game();
};
