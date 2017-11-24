import {Platform} from "../entities/platform";
import {Character} from "../entities/character";
import {SnowEffect} from "../effects/snow.ts";


export class Level {

  groundPhysicsGroup : Phaser.Group;
  platforms : Platform[] = [];
  snowEffect : SnowEffect;

  timeBetweenPlatformSpawns : number = 2200;
  time : number = this.timeBetweenPlatformSpawns;

  tileSize : number = 45;
  canReachPreviousPlatform : boolean = false;
  heightOfPreviousPlatform : number;

  constructor (
    public game : Phaser.Game,
    public character : Character
  ) {
    // Load sprite
    this.groundPhysicsGroup = this.game.add.physicsGroup();
    this.snowEffect = new SnowEffect(this.game)
    this.createPlatform(this.game.width / 3, this.game.height - 90, this.tileSize * 10);
    this.heightOfPreviousPlatform = this.game.height - this.tileSize;
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

  // NOTE: The calculations in this method look weird because
  // Y coordinates start at 0 at the top and increase towards the bottom!
  spawnPlatforms() {
    const doubleJumpHeight = 340;
    const lowest = this.game.height - this.tileSize;
    const heighest = this.tileSize * 2;
    const heighestPossible = Math.max(heighest, lowest - doubleJumpHeight);

    this.time += this.game.time.elapsed;

    if( this.time > this.timeBetweenPlatformSpawns ) {

      let randomY = 0;
      if (!this.canReachPreviousPlatform) {
        randomY = lowest - Math.random() * (lowest - heighestPossible);
        this.heightOfPreviousPlatform = randomY;
          this.canReachPreviousPlatform = true;
      } else {
        randomY = lowest - Math.random() * (lowest - heighest);
        if (randomY > heighestPossible) {
          this.heightOfPreviousPlatform = randomY;
          this.canReachPreviousPlatform = true;
        } else {
          this.canReachPreviousPlatform = false;
        }
      }

      this.createPlatform(this.game.width, randomY);
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
