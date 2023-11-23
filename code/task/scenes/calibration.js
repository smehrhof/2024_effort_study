var SceneCalibration = new Phaser.Class({
    Extends: Phaser.Scene,
    initialize: function() {
        Phaser.Scene.call(this, { "key": "SceneCalibration" });
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

        // stars
        group = this.add.group({ key: 'star', frame: 0, repeat: 230 });
        group.scaleXY(0.5, 0.5);
        rect = new Phaser.Geom.Rectangle(window.innerWidth*0.05, window.innerHeight*0.2, window.innerWidth*0.9, window.innerHeight*0.7);
        //Phaser.Actions.RandomRectangle(group.getChildren(), rect);
        this.stars = group.getChildren();
        Phaser.Actions.RandomRectangle(this.stars, rect);
        group.children.iterate((child) => {
            child.setVisible(false);
        });

        // star count and record
        this.countText = this.add.text(window.innerWidth*0.1, window.innerHeight*0.1,
            `0 Stars`, {
            fontSize: 25*globalThis.scaleFactor, color: "#000000", lineSpacing: 5, align: 'center',
        }).setOrigin(0.5, 0.5);

        if(globalThis.instructionCount > 4 && globalThis.instructionCount < 27){
            this.recordText = this.add.text(window.innerWidth*0.3, window.innerHeight*0.1,
                `Record: ` + Math.max.apply(Math, globalThis.data.clicks), {
                fontSize: 25*globalThis.scaleFactor, color: "#000000", lineSpacing: 5, align: 'center',
            }).setOrigin(0.5, 0.5);
        } else if(globalThis.instructionCount > 27){
            this.recordText = this.add.text(window.innerWidth*0.3, window.innerHeight*0.1,
                `Record: ` + Math.max.apply(Math, globalThis.data.clicks.slice(1,2)), {
                fontSize: 25*globalThis.scaleFactor, color: "#000000", lineSpacing: 5, align: 'center',
            }).setOrigin(0.5, 0.5);
        }

        // time bar 
        this.timeBarBox = this.add.rectangle((window.innerWidth*0.025), (window.innerHeight*0.043), window.innerWidth*0.95, 5*globalThis.scaleFactor).setOrigin(0,0.5);
        this.timeBarBox.setFillStyle(0xFFFEFE);
        this.timeBarFill = this.add.rectangle((window.innerWidth*0.025), (window.innerHeight*0.043), window.innerWidth*0.95, 5*globalThis.scaleFactor).setOrigin(0,0.5);
        this.timeBarFill.setFillStyle(0xFE8F31);
        var intitialTimeBar = 0.95;
        this.timeBarFill.displayWidth = window.innerWidth*intitialTimeBar;
        this.time.addEvent({
            delay: 100,
            repeat: 100,
            callback: () => this.updateTimeBar(intitialTimeBar-=0.0095)
        });

        // faster prompt
        this.fasterText = this.add.text(window.innerWidth*0.5, window.innerHeight*0.4,
            ``, {
            fontSize: 50*globalThis.scaleFactor, color: "#000000", lineSpacing: 5, align: 'center',
        }).setOrigin(0.5, 0.5);
        this.time.addEvent({
            delay: 3000,
            repeat: 1,
            callback: () => this.fasterPrompt()
        });

        // make screen clickable
        var clickCount = 0;
        cursors = this.input.keyboard.createCursorKeys();

        this.input.on('pointerdown', () => this.updateClick(++clickCount));

        this.time.addEvent({
            delay: 10000,
            callback: () => {
                globalThis.data.clicks.push(clickCount);
                ++globalThis.instructionCount;
                this.scene.start("SceneInstructions", {});
            },
            loop: false      
        });
    },

    updateTimeBar: function(intitialTimeBar) {
        this.timeBarFill.displayWidth = window.innerWidth*intitialTimeBar;
    },

    fasterPrompt: function() {
        this.fasterText.setText('Faster')
        this.time.addEvent({
            delay: 750,
            loop: false,
            callback: () => this.fasterPromptStop()
        });
    },

    fasterPromptStop: function() {
        this.fasterText.setText('')
    },

    updateClick: function(clickCount) {
        this.countText.setText(clickCount + ` Stars`);
        var star = Phaser.Utils.Array.RemoveRandomElement(this.stars);
        if (star){
            star.setVisible(true);
        }
    },

    update: function() {
        
    }
});