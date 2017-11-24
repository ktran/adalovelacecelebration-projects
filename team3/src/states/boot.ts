import * as WebFontLoader from 'webfontloader';

export class BootState extends Phaser.State {
  stage: Phaser.Stage;
  fontsReady: boolean;
  fontsLoaded: () => void;

  init () {
    this.physics.startSystem(Phaser.Physics.ARCADE);
    this.physics.arcade.gravity.y = 1000;
    this.fontsLoaded = () => { this.fontsReady = true; }
  }

  preload () {
    WebFontLoader.load({
      google: {
        families: ['Nunito']
      },
      active: this.fontsLoaded
    });

    let text = this.add.text(
        this.world.centerX,
        this.world.centerY,
        'loading fonts',
        { font: '16px Arial', fill: '#dddddd', align: 'center' }
    );
    text.anchor.setTo(0.5, 0.5);

    this.load.image('ground', './assets/images/cloud.png');
    this.load.image('background', 'assets/images/sky.png');
    this.load.image('snowflake', 'assets/images/snowflake.png');
    this.load.spritesheet('snowflakes', 'assets/images/snowflakes.png',17,17);
    this.load.spritesheet('snowflakes_large', 'assets/images/snowflakes_large.png',64,64);

    this.load.audio("jump1", "assets/sounds/jump1.wav");
    this.load.audio("jump2", "assets/sounds/jump2.wav");
    this.load.audio("jump3", "assets/sounds/jump3.wav");
    this.load.audio("jump4", "assets/sounds/jump4.wav");
    this.load.audio("soundtrack", "assets/sounds/epic_soundtrack.mp3");

  }

  render () {
    if (this.fontsReady) {
      this.game.state.start('Splash');
    }
  }
}
