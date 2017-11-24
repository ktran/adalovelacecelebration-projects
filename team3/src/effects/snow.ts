export class SnowEffect {
  max = 0;
  front_emitter;
  mid_emitter;
  back_emitter;
  update_interval = 4 * 60;
  i = 0;

  constructor (
    public game : Phaser.Game,
  ) {
    // Load sprite
    this.init_snow()
  }

  update() {
    this.i++;

    if (this.i === this.update_interval)
    {
      this.changeWindDirection();
      this.update_interval = Math.floor(Math.random() * 20) * 60; // 0 - 20sec @ 60fps
      this.i = 0;
    }
  }

  init_snow() {
    this.back_emitter = this.game.add.emitter(this.game.world.centerX, -32, 600);
    this.back_emitter.makeParticles('snowflakes', [0, 1, 2, 3, 4, 5]);
    this.back_emitter.maxParticleScale = 0.6;
    this.back_emitter.minParticleScale = 0.2;
    this.back_emitter.setYSpeed(20, 100);
    this.back_emitter.gravity = 0;
    this.back_emitter.width = this.game.world.width * 1.5;
    this.back_emitter.minRotation = 0;
    this.back_emitter.maxRotation = 40;

    this.mid_emitter = this.game.add.emitter(this.game.world.centerX, -32, 250);
    this.mid_emitter.makeParticles('snowflakes', [0, 1, 2, 3, 4, 5]);
    this.mid_emitter.maxParticleScale = 1.2;
    this.mid_emitter.minParticleScale = 0.8;
    this.mid_emitter.setYSpeed(50, 150);
    this.mid_emitter.gravity = 0;
    this.mid_emitter.width = this.game.world.width * 1.5;
    this.mid_emitter.minRotation = 0;
    this.mid_emitter.maxRotation = 40;

    this.front_emitter = this.game.add.emitter(this.game.world.centerX, -32, 50);
    this.front_emitter.makeParticles('snowflakes_large', [0, 1, 2, 3, 4, 5]);
    this.front_emitter.maxParticleScale = 1;
    this.front_emitter.minParticleScale = 0.5;
    this.front_emitter.setYSpeed(100, 200);
    this.front_emitter.gravity = 0;
    this.front_emitter.width = this.game.world.width * 1.5;
    this.front_emitter.minRotation = 0;
    this.front_emitter.maxRotation = 40;

    this.changeWindDirection();

    this.back_emitter.start(false, 14000, 20);
    this.mid_emitter.start(false, 12000, 40);
    this.front_emitter.start(false, 6000, 1000);
  }

  changeWindDirection() {

    let multi = Math.floor((this.max + 200) / 4);
    let frag = (Math.floor(Math.random() * 100) - multi);
    this.max = this.max + frag;

    if (this.max > 200) this.max = 150;
    if (this.max < -200) this.max = -150;

    this.setXSpeed(this.back_emitter, this.max);
    this.setXSpeed(this.mid_emitter, this.max);
    this.setXSpeed(this.front_emitter, this.max);

  }

  setXSpeed(emitter, max) {

    emitter.setXSpeed(max - 20, max);
    emitter.forEachAlive(this.setParticleXSpeed, this, max);

  }

  setParticleXSpeed(particle, max) {

    particle.body.velocity.x = max - Math.floor(Math.random() * 30);

  }

}
