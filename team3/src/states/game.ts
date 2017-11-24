import {Character} from "../entities/character";
import {Platform} from "../entities/platform";
import {Level} from "../entities/level";

export class GameState extends Phaser.State {
  character: Character;
  level : Level;
  timer: Phaser.Text;
  banner: Phaser.Text;
  soundtrack: any;

  init () {}
  preload () {}

  create () {
    let background = this.add.tileSprite(0, 0, 1024, 576, 'background');
    background.fixedToCamera = true;
    this.banner = this.add.text(350, 20, "You must survive!", {});
    this.timer = this.add.text(70, 20, "0", {});
    this.banner.font = 'Nunito';
    this.banner.fontSize = 40;
    this.banner.fill = '#ffffff';
    this.banner.align = "center";45
    this.banner.anchor.setTo(0, 0);
    this.timer.font = 'Nunito';
    this.timer.fontSize = 40;
    this.timer.fill = '#ffffff';
    this.timer.align = "center";
    this.timer.anchor.setTo(0, 0);

    this.character = new Character(this.game, this.physics, 45, 45);
    this.level = new Level(this.game, this.character);

    this.time.reset();
}

  render () {}

  update() {
    this.timer.setText(this.time.totalElapsedSeconds().toFixed(2).toString());
    this.level.update();
    if(this.character.isDead){
        this.banner.setText("You died. Press 'up' to try again.");
        this.banner.fill = '#b70000';
        let cursors = this.game.input.keyboard.createCursorKeys();
        cursors.up.onDown.add( () => {
            this.game.state.restart();
            this.game.paused = false;
        });
    }
  }
}
