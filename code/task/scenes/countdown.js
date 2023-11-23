var SceneCountdown = new Phaser.Class({
    Extends: Phaser.Scene,
    initialize: function() {
        Phaser.Scene.call(this, { "key": "SceneCountdown" });
    },

    init: function() {
    },

    preload: function() {
        // background images
        this.load.image('water', 'assets/undersea.jpeg');
        this.load.image('seabed', 'assets/seabed.png');
    },
    
    create: function() {
        // add background and scale to browser window 
        background = this.add.image(window.innerWidth*0.5, window.innerHeight*0.5, 'water');
        var scaleX = window.innerWidth / background.width;
        var scaleY = window.innerHeight / background.height;
        background.setScale(scaleX, scaleY);
        seabed = this.add.image(window.innerWidth*0.5, window.innerHeight, 'seabed').setOrigin(0.5,1);
        seabed.setScale(scaleX, scaleY);
        
        if(!(globalThis.data.choice[globalThis.data.choice.length-1] == 0)){
            this.add.text(window.innerWidth*0.5, window.innerHeight*0.2,
                `Start clicking in`, {
                fontSize: 50*globalThis.scaleFactor, color: "#000000", lineSpacing: 5, align: 'center',
            }).setOrigin(0.5, 0.5);
        }

        var initialTime = 3;
        this.countText = this.add.text(window.innerWidth*0.5, window.innerHeight*0.5,
            initialTime, {
            fontSize: 140*globalThis.scaleFactor, color: "#000000", lineSpacing: 5, align: 'center',
        }).setOrigin(0.5, 0.5);

        // countdown in steps of 1sec
        this.time.addEvent({
            delay: 500,
            repeat: 3,
            callback: () => this.updateTime(--initialTime)
        });
    
        if(globalThis.data.phase[globalThis.data.phase.length-1] == 'calibration'){
            this.time.addEvent({
                delay: 2000,
                callback: () => {
                    this.scene.start('SceneCalibration', {});
                },
                loop: false      
            });
        } else {
            this.time.addEvent({
                delay: 2000,
                callback: () => {
                    this.scene.start('SceneGame', {});
                },
                loop: false      
            });
        }
    },

    // to update countdown
    updateTime: function(initialTime) {
        if(initialTime > 0){
            this.countText.setText(initialTime);
        } else  {
            this.countText.setText("GO");
        }
    },

    update: function() {
        
    }
});