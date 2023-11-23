var SceneGame = new Phaser.Class({
    Extends: Phaser.Scene,
    initialize: function() {
        Phaser.Scene.call(this, { "key": "SceneGame" });
    },

    init: function() {
    },

    preload: function() {
    },
    
    create: function() {
        // add background and scale to browser window 
        background = this.add.image(window.innerWidth*0.5, window.innerHeight*0.5, 'water');
        var scaleX = window.innerWidth / background.width;
        var scaleY = window.innerHeight / background.height;
        background.setScale(scaleX, scaleY);
        seabed = this.add.image(window.innerWidth*0.5, window.innerHeight, 'seabed').setOrigin(0.5,1);
        seabed.setScale(scaleX, scaleY);

        // squid speed is calibration dependent
        // set so that they would cross 95% of screen in 20 sec at max speed 
        this.moveClick = (95 / (20 * globalThis.clickSpeed))/100;

        // level dependent variables
        if(globalThis.data.offerEffort[globalThis.data.offerEffort.length-1] == 1){
            this.timer = 8;
            this.capacity = 0.3;
        } else if(globalThis.data.offerEffort[globalThis.data.offerEffort.length-1] == 2){
            this.timer = 11;
            this.capacity = 0.5;
        } else if(globalThis.data.offerEffort[globalThis.data.offerEffort.length-1] == 3){
            this.timer = 14;
            this.capacity = 0.7;
        } else if(globalThis.data.offerEffort[globalThis.data.offerEffort.length-1] == 4){
            this.timer = 17;
            this.capacity = 0.9;
        }

        // if they rejected the offer, wait screen instead of playing
        if(globalThis.data.choice[globalThis.data.choice.length-1] == 0){
            globalThis.data.goalClicks.push(999);
            globalThis.data.clicks.push(999)

            this.squid = this.physics.add.sprite(window.innerWidth*0.5, window.innerHeight*0.45, 'squid');
            this.squid.setScale(1.5 * scaleFactor);
            this.squid.setOrigin(0.5, 0.5);
            this.anims.create({
                key: 'squidswim',
                frames: this.anims.generateFrameNumbers('squid', { start: 0, end: 4 }),
                frameRate: 7,
                repeat: 0,
                yoyo: true
            });

            // make background clickable 
            cursors = this.input.keyboard.createCursorKeys();
            this.input.on('pointerdown', () => this.updateWait());

            this.time.addEvent({
                delay: this.timer*1000,
                callback: () => {
                    this.scene.start("SceneFeedback", {})},
                loop: false      
            });

        // if they accept the offer (and in all practice trials)
        } else {
            this.clicksGoal = Phaser.Math.RoundTo((globalThis.clickSpeed*this.capacity)*this.timer, 0);
            globalThis.data.goalClicks.push(this.clicksGoal);
            this.placeGoal = (this.moveClick*this.clicksGoal+0.05);
            this.shrimpSpeed = (window.innerWidth*this.placeGoal-window.innerWidth*0.1)/this.timer
            this.time.addEvent({
                delay: this.timer*1000,
                callback: () => {
                    globalThis.data.clicks.push(clickCount)
                    this.scene.start("SceneFeedback", {})},
                loop: false      
            });
    
            // avatars
            // squid
            var squidPositionX = 0.05;
            this.squid = this.physics.add.sprite(window.innerWidth*squidPositionX, window.innerHeight*0.45, 'squid');
            this.squid.setScale(1.5 * scaleFactor);
            this.squid.setOrigin(0.5, 0.5);
            this.anims.create({
                key: 'squidswim',
                frames: this.anims.generateFrameNumbers('squid', { start: 0, end: 4 }),
                frameRate: 7,
                repeat: 0,
                yoyo: true
            });
            this.squid.setVelocityX(0);
            this.squid.setCollideWorldBounds(true);
            // shrimp
            this.shrimp = this.physics.add.sprite(window.innerWidth*0.1, window.innerHeight*0.375, 'shrimp');
            this.shrimp.setScale(1 * scaleFactor);
            this.shrimp.setOrigin(0, 0.5);
            this.anims.create({
                key: 'shrimpswim',
                frames: this.anims.generateFrameNumbers('shrimp', { start: 0, end: 1 }),
                frameRate: 7,
                repeat: -1,
                yoyo: true
            });
            this.shrimp.anims.play('shrimpswim', true);
            this.shrimp.setVelocityX(this.shrimpSpeed);
            this.shrimp.setCollideWorldBounds(true);
    
            // click counter 
            var clickCount = 0;
    
            // make background clickable 
            cursors = this.input.keyboard.createCursorKeys();
            this.input.on('pointerdown', () => this.updateClick(++clickCount, squidPositionX+=this.moveClick));
        }

        // time bar 
        this.timeBarBox = this.add.rectangle((window.innerWidth*0.025), (window.innerHeight*0.043), window.innerWidth*0.95, 5*globalThis.scaleFactor).setOrigin(0,0.5);
        this.timeBarBox.setFillStyle(0xFFFEFE);
        this.timeBarFill = this.add.rectangle((window.innerWidth*0.025), (window.innerHeight*0.043), window.innerWidth*0.95, 5*globalThis.scaleFactor).setOrigin(0,0.5);
        this.timeBarFill.setFillStyle(0xFE8F31);
        var intitialTimeBar = 0.95;
        this.timeBarFill.displayWidth = window.innerWidth*intitialTimeBar;
    
        this.time.addEvent({
            delay: this.timer*10,
            repeat: 100,
            callback: () => this.updateTimeBar(intitialTimeBar-=0.0095)
        });

        // text indicating they are close enough
        this.winText = this.add.text(window.innerWidth*0.5, window.innerHeight*0.1,
            ``, {
            fontSize: 25*globalThis.scaleFactor, color: "#000000", lineSpacing: 5, align: 'center',
        }).setOrigin(0.5, 0.5);

    },
    
    updateTimeBar: function(intitialTimeBar) {
        this.timeBarFill.displayWidth = window.innerWidth*intitialTimeBar;
        if(globalThis.data.choice[globalThis.data.choice.length-1] == 0){
            this.winText.setText(``);
        } else {
            if(Math.floor(this.shrimp.x - this.squid.x) > 0){
                this.winText.setText(``);
            } else {
                this.winText.setText(`Good job! Keep it up`);
            };
        };
    },

    updateClick: function(clickCount, squidPositionX) {
        this.squid.x = window.innerWidth*squidPositionX;
        this.squid.anims.play('squidswim', true);
    },

    updateWait: function() {
        this.squid.anims.play('squidswim', true);
    },

    update: function() {
        
    }
});