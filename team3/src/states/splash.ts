export class SplashState extends Phaser.State {
  loaderBg: Phaser.Sprite;
  loaderBar: Phaser.Sprite;

  cursors: Phaser.CursorKeys;
  banner: Phaser.Text;

  init () {}

  preload () {
    this.loaderBg = this.add.sprite(this.game.world.centerX, this.game.world.centerY, 'loaderBg');
    this.loaderBar = this.add.sprite(this.game.world.centerX, this.game.world.centerY, 'loaderBar');

    this.load.setPreloadSprite(this.loaderBar);
    //
    // load your assets
    //
    this.load.spritesheet('runner', './assets/images/runner_spritesheet.png', 50, 50);

  }

  create () {

    let background = this.add.tileSprite(0, 0, 1024, 576, 'background');
    background.fixedToCamera = true;
    var title = this.add.text(100, 120, "You must survive...", {});
    title.font = 'Nunito';
    title.fontSize = 40;
    title.fill = '#b70000';
    title.align = "center";
    this.banner = this.add.text(100, this.game.height / 2, "Press UP to play!", {});
    this.banner.font = 'Nunito';
    this.banner.fontSize = 20;
    this.banner.fill = '#ffffff';
    this.banner.align = "center";

    let soundtrack = this.game.add.audio("soundtrack");
    soundtrack.volume = 0.1;
    soundtrack.play();

    this.cursors = this.game.input.keyboard.createCursorKeys();
    // this.game.state.start('Game');

    this.cursors.up.onDown.add( () => { this.game.state.start('Game'); });
  }
}
