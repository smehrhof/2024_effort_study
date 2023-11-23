var SceneOffer = new Phaser.Class({
    Extends: Phaser.Scene,
    initialize: function() {
        Phaser.Scene.call(this, { "key": "SceneOffer" });
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

        // pratice phase 
        if(globalThis.data.phase[globalThis.data.phase.length-1] == 'practice'){
            // add effort level to practice 
            this.add.text(window.innerWidth*0.5, window.innerHeight*0.1, 
                `Effort level: ` + globalThis.data.trial[globalThis.data.trial.length-1] + `/4`, {
                    fontSize: 30*globalThis.scaleFactor, color: "#000000", lineSpacing: 5,
            }).setOrigin(0.5, 0);
            if(globalThis.data.trial[globalThis.data.trial.length-1] == 1){
                this.add.image(window.innerWidth*0.5, window.innerHeight*0.35, 'effort1'
                    ).setScale(1*0.15*globalThis.scaleFactor);
                globalThis.data.offerEffort.push(1);
            } else if(globalThis.data.trial[globalThis.data.trial.length-1] == 2){
                this.add.image(window.innerWidth*0.5, window.innerHeight*0.35, 'effort2'
                    ).setScale(1*0.15*globalThis.scaleFactor);
                    globalThis.data.offerEffort.push(2);    
            } else if(globalThis.data.trial[globalThis.data.trial.length-1] == 3){
                this.add.image(window.innerWidth*0.5, window.innerHeight*0.35, 'effort3'
                    ).setScale(1*0.15*globalThis.scaleFactor);
                globalThis.data.offerEffort.push(3);
            } else if(globalThis.data.trial[globalThis.data.trial.length-1] == 4){
                this.add.image(window.innerWidth*0.5, window.innerHeight*0.35, 'effort4'
                    ).setScale(1*0.15*globalThis.scaleFactor);
                    globalThis.data.offerEffort.push(4);
            } 

            // press play to practice text
            this.add.text(window.innerWidth*0.5, window.innerHeight*0.575, 
                `Press Play to practice this level`, {
                fontSize: 35*globalThis.scaleFactor, color: "#000000",
            }).setOrigin(0.5, 0);

            // play button
            this.playButton = this.add.graphics()
            this.playButton.fillRoundedRect((window.innerWidth*0.5)-90*globalThis.scaleFactor, (window.innerHeight*0.75)-30*globalThis.scaleFactor, 180*globalThis.scaleFactor, 60*globalThis.scaleFactor, 10, 0xFE8F31, 1);
            this.playText = this.add.text(window.innerWidth*0.5, window.innerHeight*0.75,
                `Play`, {
                    fontSize: 40*globalThis.scaleFactor, color: "white", 
                    backgroundColor: "#FE8F31", padding: {
                        left: 40*globalThis.scaleFactor, right: 40*globalThis.scaleFactor, top: 4*globalThis.scaleFactor, bottom: 4*globalThis.scaleFactor,
                    }}).setOrigin(0.5);

            this.playText.setInteractive({ useHandCursor: true });
            this.playText.on('pointerdown', () => {
                this.scene.start("SceneCountdown", {});
            });

        // game phase     
        } else if(globalThis.data.phase[globalThis.data.phase.length-1] == 'game'){

        // stop time
        start = new Date();
        this.startTime = start.getTime();

        // choose random trial type
        this.currentTrial = Phaser.Utils.Array.RemoveRandomElement(globalThis.trialsArray[0]);
        globalThis.data.trialType.push(this.currentTrial);
        globalThis.data.offerEffort.push(globalThis.trialsArray[1][this.currentTrial][0]);
        globalThis.data.offerReward.push(globalThis.trialsArray[1][this.currentTrial][1]);

        // text
        this.add.text(window.innerWidth*0.5, window.innerHeight*0.49, 
            `Do you accept the challenge?`, {
            fontSize: 30*globalThis.scaleFactor, color: "#000000", align: 'center',
        }).setOrigin(0.5, 0);

        // effort
        this.add.text(window.innerWidth*0.7, window.innerHeight*0.08, 
            `Effort level: ` + globalThis.data.offerEffort[globalThis.data.offerEffort.length-1] + `/4`, {
            fontSize: 25*globalThis.scaleFactor, color: "#000000", lineSpacing: 5,
        }).setOrigin(0.5, 0);
        if(globalThis.data.offerEffort[globalThis.data.offerEffort.length-1] == 1){
            this.add.image(window.innerWidth*0.7, window.innerHeight*0.28, 'effort1'
                ).setScale(0.11*globalThis.scaleFactor);
        } else if(globalThis.data.offerEffort[globalThis.data.offerEffort.length-1] == 2){
            this.add.image(window.innerWidth*0.7, window.innerHeight*0.28, 'effort2'
                ).setScale(0.11*globalThis.scaleFactor);
        } else if(globalThis.data.offerEffort[globalThis.data.offerEffort.length-1] == 3){
            this.add.image(window.innerWidth*0.7, window.innerHeight*0.28, 'effort3'
                ).setScale(0.11*globalThis.scaleFactor);
        } else if(globalThis.data.offerEffort[globalThis.data.offerEffort.length-1] == 4){
            this.add.image(window.innerWidth*0.7, window.innerHeight*0.28, 'effort4'
                ).setScale(0.11*globalThis.scaleFactor);
        } 

        // reward
        this.add.text(window.innerWidth*0.3, window.innerHeight*0.08, 
            `Points to win: ` + (globalThis.data.offerReward[globalThis.data.offerReward.length-1]+1) + `/5`, {
            fontSize: 25*globalThis.scaleFactor, color: "#000000", lineSpacing: 5,
        }).setOrigin(0.5, 0);
        if(globalThis.data.offerReward[globalThis.data.offerReward.length-1] == 1){
            this.add.image(window.innerWidth*0.3, window.innerHeight*0.28, 'reward1'
            ).setScale(0.12*globalThis.scaleFactor);
        } else if(globalThis.data.offerReward[globalThis.data.offerReward.length-1] == 2){
            this.add.image(window.innerWidth*0.3, window.innerHeight*0.28, 'reward2'
            ).setScale(0.12*globalThis.scaleFactor);
        } else if(globalThis.data.offerReward[globalThis.data.offerReward.length-1] == 3){
            this.add.image(window.innerWidth*0.3, window.innerHeight*0.28, 'reward3'
            ).setScale(0.12*globalThis.scaleFactor);
        } else if(globalThis.data.offerReward[globalThis.data.offerReward.length-1] == 4){
            this.add.image(window.innerWidth*0.3, window.innerHeight*0.28, 'reward4'
            ).setScale(0.12*globalThis.scaleFactor);
        } 

        // accept button
        var acceptButton = this.add.graphics();
        acceptButton.fillStyle(0x96B81C, 1);
        acceptButton.fillRoundedRect((window.innerWidth*0.3)-135*globalThis.scaleFactor, (window.innerHeight*0.68)-30*globalThis.scaleFactor, 270*globalThis.scaleFactor, 60*globalThis.scaleFactor, 10);
        var acceptText = this.add.text(window.innerWidth*0.3, window.innerHeight*0.68,
            "Accept", {
                fontSize: 40*globalThis.scaleFactor, color: "white", 
                backgroundColor: "#96B81C", padding: {
                    left: 60*globalThis.scaleFactor, right: 60*globalThis.scaleFactor, top: 4*globalThis.scaleFactor, bottom: 4*globalThis.scaleFactor,
                }}).setOrigin(0.5);

        this.time.addEvent({
            delay: 1000,
            loop: false,
            callback: () => {
                acceptText.setInteractive({ useHandCursor: true });
                acceptText.on('pointerdown', () => {
                    acceptText.setInteractive(false);
                    globalThis.data.choice.push(1)
                    this.updateAccept()
                    this.scene.start("SceneCountdown", {});
                });
            }
        });

        this.add.text(window.innerWidth*0.3, window.innerHeight*0.78, 
            `Win ` + (globalThis.data.offerReward[globalThis.data.offerReward.length-1]+1) + ` points`, {
            fontSize: 25*globalThis.scaleFactor, color: "#000000", align: 'center',
        }).setOrigin(0.5, 0);

        // reject button
        var rejectButton = this.add.graphics();
        rejectButton.fillStyle(0xE73948, 1);
        rejectButton.fillRoundedRect((window.innerWidth*0.7)-135*globalThis.scaleFactor, (window.innerHeight*0.68)-30*globalThis.scaleFactor, 270*globalThis.scaleFactor, 60*globalThis.scaleFactor, 10);
        var rejectText = this.add.text(window.innerWidth*0.7, window.innerHeight*0.68,
            "Reject", {
                fontSize: 40*globalThis.scaleFactor, color: "white", 
                backgroundColor: "#E73948", padding: {
                    left: 60*globalThis.scaleFactor, right: 60*globalThis.scaleFactor, top: 4*globalThis.scaleFactor, bottom: 4*globalThis.scaleFactor,
                }}).setOrigin(0.5);


        this.time.addEvent({
            delay: 1000,
            loop: false,
            callback: () => {
                rejectText.setInteractive({ useHandCursor: true });
                rejectText.on('pointerdown', () => {
                    rejectText.setInteractive(false);
                    globalThis.data.choice.push(0)
                    this.updateReject()
                    this.scene.start("SceneCountdown", {});
                });
            }
        });
        


        this.add.text(window.innerWidth*0.7, window.innerHeight*0.78, 
            `Get 1 point`, {
            fontSize: 25*globalThis.scaleFactor, color: "#000000", align: 'center',
        }).setOrigin(0.5, 0);

        }

    },

    updateAccept: function() {
        // update staircase for next trial: increase effort or decrease reward
        // if only either can be updated, chose that
        if(globalThis.data.offerEffort[globalThis.data.offerEffort.length-1] == 4 && globalThis.data.offerReward[globalThis.data.offerReward.length-1] == 1){
            this.adjust = 'n';
        } else if(globalThis.data.offerEffort[globalThis.data.offerEffort.length-1] == 4){
            this.adjust = 'r';
        } else if (globalThis.data.offerReward[globalThis.data.offerReward.length-1] == 1) {
            this.adjust = 'e';
        } else {
            this.adjust = globalThis.trialsArray[2][this.currentTrial];
        }
        
        if(this.adjust == 'n'){
            globalThis.trialsArray[1][this.currentTrial] = globalThis.trialsArray[1][this.currentTrial]
        // increase effort
        } else if(this.adjust == 'e'){
            globalThis.trialsArray[1][this.currentTrial][0] = globalThis.data.offerEffort[globalThis.data.offerEffort.length-1] + 1
            globalThis.trialsArray[2][this.currentTrial] = 'r'
        // decrease reward 
        } else if(this.adjust == 'r'){
            globalThis.trialsArray[1][this.currentTrial][1] = globalThis.data.offerReward[globalThis.data.offerReward.length-1] - 1
            globalThis.trialsArray[2][this.currentTrial] = 'e'
        } 
 
        end = new Date();
        this.endTime = end.getTime();

        globalThis.data.rt.push(this.endTime - this.startTime);
    },

    updateReject: function() {
        // update staircase for next trial: decrease effort or increase reward
        // if only either can be updated, chose that
        if(globalThis.data.offerEffort[globalThis.data.offerEffort.length-1] == 1 && globalThis.data.offerReward[globalThis.data.offerReward.length-1] == 4){
            this.adjust = 'n';
        } else if (globalThis.data.offerEffort[globalThis.data.offerEffort.length-1] == 1){
            this.adjust = 'r';
        } else if (globalThis.data.offerReward[globalThis.data.offerReward.length-1] == 4) {
            this.adjust = 'e';
        } else {
            this.adjust = globalThis.trialsArray[2][this.currentTrial];
        }
       
        if(this.adjust == 'n'){
            globalThis.trialsArray[1][this.currentTrial] = globalThis.trialsArray[1][this.currentTrial]
        // decrease effort 
        } else if(this.adjust == 'e'){
            globalThis.trialsArray[1][this.currentTrial][0] = globalThis.data.offerEffort[globalThis.data.offerEffort.length-1] - 1
            globalThis.trialsArray[2][this.currentTrial] = 'r'
        // increase reward 
        } else if(this.adjust == 'r'){
            globalThis.trialsArray[1][this.currentTrial][1] = globalThis.data.offerReward[globalThis.data.offerReward.length-1] + 1
            globalThis.trialsArray[2][this.currentTrial] = 'e'
        } 
        
        end = new Date();
        this.endTime = end.getTime();

        globalThis.data.rt.push(this.endTime - this.startTime);
    },

    update: function() {
        
    }
});