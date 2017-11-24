import {Platform} from "../entities/platform";
import {Character} from "../entities/character";
import {SnowEffect} from "../effects/snow.ts";


export class Level {

  groundPhysicsGroup : Phaser.Group;
  platforms : Platform[] = [];
  time : number = 0;
  snowEffect : SnowEffect;

  lastPlatform : Platform;
  tileSize : number = 45;

  constructor (
    public game : Phaser.Game,
    public character : Character
  ) {
    // Load sprite
    this.groundPhysicsGroup = this.game.add.physicsGroup();
    this.snowEffect = new SnowEffect(this.game)
    this.createPlatform(this.game.width / 2, this.game.height - 90, this.tileSize * 10);
  }

  createPlatform(x : number, y : number, w?: number) {

    let minWidth = this.tileSize * 3;
    let maxWidth = this.tileSize * 6;

    let width = minWidth + (maxWidth - minWidth) * Math.random();
    if(w) {
      width = w;
    }

    let fall = (Math.random() > 0.2)

    let platform = new Platform(this.game, this.groundPhysicsGroup, x, y, width, this.tileSize, "ground", fall);
    this.platforms.push(platform)
    return platform;
  }

  spawnPlatforms() {
    this.time += this.game.time.elapsed;


    if( this.time > 2200 || this.lastPlatform == null ) {
       const randomY = Math.max(this.tileSize * 2, Math.random() * (this.game.height - this.tileSize));

      this.lastPlatform = this.createPlatform(this.game.width, randomY);
      this.time = 0;
    }


  }

  update () {
    // Happens every frame
    this.game.physics.arcade.collide(this.character.sprite, this.groundPhysicsGroup);
    this.game.physics.arcade.collide(this.groundPhysicsGroup, this.character.sprite);
    this.character.update();
    this.platforms = this.platforms.filter(platform => platform.update());

    this.spawnPlatforms();

  }
}
