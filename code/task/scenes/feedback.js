var SceneFeedback = new Phaser.Class({
    Extends: Phaser.Scene,
    initialize: function() {
        Phaser.Scene.call(this, { "key": "SceneFeedback" });
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

        this.feedbackText = this.add.text(window.innerWidth*0.5, window.innerHeight*0.25, 
            `` , {
                fontSize: 60*globalThis.scaleFactor, color: "#000000", lineSpacing: 5, align: 'center',
        }).setOrigin(0.5, 0.5);

        this.earningsText = this.add.text(window.innerWidth*0.5, window.innerHeight*0.4, 
            `` , {
                fontSize: 40*globalThis.scaleFactor, color: "#000000", lineSpacing: 5, align: 'center',
        }).setOrigin(0.5, 0.5);

        this.pointsBank = this.add.text(window.innerWidth*0.5, window.innerHeight*0.7, 
            `` , {
                fontSize: 40*globalThis.scaleFactor, color: "#000000", lineSpacing: 5, align: 'center',
        }).setOrigin(0.5, 0.5);

        // next button
        this.nextButton = this.add.graphics();
        this.nextButton.fillStyle(0xFE8F31, 1);
        this.nextButton.fillRoundedRect((window.innerWidth*0.5)-90*globalThis.scaleFactor, (window.innerHeight*0.75)-30*globalThis.scaleFactor, 180*globalThis.scaleFactor, 60*globalThis.scaleFactor, 10);
        this.nextButton.setVisible(false);
        this.nextText = this.add.text(window.innerWidth*0.5, window.innerHeight*0.75,
            "", {
                fontSize: 40*globalThis.scaleFactor, color: "white", 
                backgroundColor: "#FE8F31", padding: {
                    left: 17*globalThis.scaleFactor, right: 17*globalThis.scaleFactor, top: 4*globalThis.scaleFactor, bottom: 4*globalThis.scaleFactor,
                }}).setOrigin(0.5);
        this.nextText.setInteractive({ useHandCursor: true });
            this.nextText.on('pointerdown', () => {
                this.scene.start("SceneInstructions", {});
        });
        this.nextText.setVisible(false);

        if(globalThis.data.phase[globalThis.data.phase.length-1] == 'quiz'){

            this.nextButton.setVisible(true);
            this.nextText.setVisible(true);

            if(globalThis.data.choice[globalThis.data.choice.length-6] == 1 &&
                globalThis.data.choice[globalThis.data.choice.length-5] == 0 &&
                globalThis.data.choice[globalThis.data.choice.length-4] == 1 &&
                globalThis.data.choice[globalThis.data.choice.length-3] == 1 &&
                globalThis.data.choice[globalThis.data.choice.length-2] == 0 &&
                globalThis.data.choice[globalThis.data.choice.length-1] == 0){

                this.feedbackText.setText(`Well done!\n You're ready to \nstart the game now`);
                this.feedbackText.setFontSize(50*globalThis.scaleFactor);
                this.feedbackText.setY(window.innerHeight*0.35);
                ++globalThis.instructionCount
                this.nextText.setText("Start")
            } else {
                this.feedbackText.setText(`That wasn't quite right!\nPlease try again`);
                this.feedbackText.setFontSize(50*globalThis.scaleFactor);
                this.feedbackText.setY(window.innerHeight*0.35);
                globalThis.instructionCount = 13;
                this.nextText.setText("Next")
            }


        } else if(globalThis.data.phase[globalThis.data.phase.length-1] == 'practice'){
            if(globalThis.data.clicks[globalThis.data.clicks.length-1] >= globalThis.data.goalClicks[globalThis.data.goalClicks.length-1]){
                this.feedbackText.setText('Well done!')
                this.feedbackText.setY(window.innerHeight*0.35);

                if(globalThis.data.offerEffort[globalThis.data.offerEffort.length-1] < 4){
                    // next offer
                    this.time.addEvent({
                        delay: 1500,
                        callback: () => {
                            globalThis.data.trial.push(globalThis.data.trial[globalThis.data.trial.length-1]+1)
                            this.scene.start("SceneOffer", {})},
                            loop: false      
                    });
                } else {
                    this.time.addEvent({
                        delay: 1500,
                        callback: () => {
                            ++globalThis.instructionCount;
                            this.scene.start("SceneInstructions", {})},
                            loop: false      
                    });
                }

            } else {
                this.feedbackText.setText('Too slow!\nTry again and\nclick faster');
                this.feedbackText.setY(window.innerHeight*0.35);

                // update click speed calibration if they can't do it
                if(globalThis.data.trial[globalThis.data.trial.length-1] == globalThis.data.trial[globalThis.data.trial.length-2]){
                    globalThis.clickSpeed = Phaser.Math.Average(globalThis.data.clicks.slice(1,2))/10;

                    if(globalThis.data.trial[globalThis.data.trial.length-1] == 1){
                        globalThis.clickSpeed = (globalThis.data.clicks[globalThis.data.clicks.length-1]/8)/0.3;

                    } else if(globalThis.data.trial[globalThis.data.trial.length-1] == 2){
                        globalThis.clickSpeed = (globalThis.data.clicks[globalThis.data.clicks.length-1]/11)/0.5;

                    } else if(globalThis.data.trial[globalThis.data.trial.length-1] == 3){
                        globalThis.clickSpeed = (globalThis.data.clicks[globalThis.data.clicks.length-1]/14)/0.7;

                    } else if(globalThis.data.trial[globalThis.data.trial.length-1] == 4){
                        globalThis.clickSpeed = (globalThis.data.clicks[globalThis.data.clicks.length-1]/17)/0.9;
                    }
                }

                globalThis.data.phase.push('practice');
                globalThis.data.trialType.push(999);
                globalThis.data.offerReward.push(999);
                globalThis.data.choice.push(999);
                globalThis.data.rt.push(999);
                globalThis.data.points.push(0);

                // same offer again
                this.time.addEvent({
                    delay: 1500,
                    callback: () => {
                        globalThis.data.trial.push(globalThis.data.trial[globalThis.data.trial.length-1])
                        this.scene.start("SceneOffer", {})},
                        loop: false      
                });
            }

        } else if(globalThis.data.phase[globalThis.data.phase.length-1] == 'game'){
            if(globalThis.data.choice[globalThis.data.choice.length-1] == 0){
                this.feedbackText.setText('You get one point')
                this.feedbackText.setY(window.innerHeight*0.35);
                globalThis.data.points.push(globalThis.data.points[globalThis.data.points.length-1] + 1)
                this.pointsBank.setText('Total: ' + (globalThis.data.points[globalThis.data.points.length-1]) + ' points')
            } else {
                if(globalThis.data.clicks[globalThis.data.clicks.length-1] >= globalThis.data.goalClicks[globalThis.data.goalClicks.length-1]){
                    this.feedbackText.setY(window.innerHeight*0.25);
                    this.earningsText.setText('You win ' + (globalThis.data.offerReward[globalThis.data.offerReward.length-1] + 1) + ' points')
                    if(globalThis.data.offerReward[globalThis.data.offerReward.length-1] == 1){
                        this.feedbackText.setText('Good');
                        this.add.image(window.innerWidth*0.5, window.innerHeight*0.55, 'winReward1'
                        ).setScale(0.75*globalThis.scaleFactor);
                    } else if(globalThis.data.offerReward[globalThis.data.offerReward.length-1] == 2){
                        this.feedbackText.setText('Well done!');
                        this.add.image(window.innerWidth*0.5, window.innerHeight*0.55, 'winReward2'
                        ).setScale(0.8*globalThis.scaleFactor);
                    } else if(globalThis.data.offerReward[globalThis.data.offerReward.length-1] == 3){
                        this.feedbackText.setText('Great job!');
                        this.add.image(window.innerWidth*0.5, window.innerHeight*0.55, 'winReward3'
                        ).setScale(0.8*globalThis.scaleFactor);
                    } else if(globalThis.data.offerReward[globalThis.data.offerReward.length-1] == 4){
                        this.feedbackText.setText('Amazing!');
                        this.add.image(window.innerWidth*0.5, window.innerHeight*0.55, 'winReward4'
                        ).setScale(0.8*globalThis.scaleFactor);
                    } else if(globalThis.data.offerReward[globalThis.data.offerReward.length-1] == 5){
                        this.feedbackText.setText('OUTSTANDING!');
                        this.add.image(window.innerWidth*0.5, window.innerHeight*0.55, 'winReward5'
                        ).setScale(0.8*globalThis.scaleFactor);
                    }

                    globalThis.data.points.push(globalThis.data.points[globalThis.data.points.length-1] + (globalThis.data.offerReward[globalThis.data.offerReward.length-1] + 1))
                    this.pointsBank.setText('Total: ' + (globalThis.data.points[globalThis.data.points.length-1]) + ' points')
                } else {
                    this.feedbackText.setText('Too slow! \nClick faster next time')
                    this.feedbackText.setY(window.innerHeight*0.35);
                    globalThis.data.points.push(globalThis.data.points[globalThis.data.points.length-1])
                }
            }

            if(globalThis.data.trial[globalThis.data.trial.length-1] == 16){
                this.time.addEvent({
                    delay: 1500,
                    callback: () => {
                        globalThis.data.trial.push(1)
                        ++globalThis.instructionCount;
                        this.scene.start("SceneInstructions", {})},
                        loop: false      
                });
            } else if(globalThis.data.trial[globalThis.data.trial.length-1] == 32) {
                this.time.addEvent({
                    delay: 1500,
                    callback: () => {
                        globalThis.data.trial.push(1)
                        ++globalThis.instructionCount;
                        this.scene.start("SceneInstructions", {})},
                        loop: false      
                });
            } else if(globalThis.data.trial[globalThis.data.trial.length-1] == 48){
                this.time.addEvent({
                    delay: 1500,
                    callback: () => {
                        globalThis.data.trial.push(1)
                        ++globalThis.instructionCount;
                        this.scene.start("SceneInstructions", {})},
                        loop: false      
                });
            } else if(globalThis.data.trial[globalThis.data.trial.length-1] == 64){
                this.time.addEvent({
                    delay: 1500,
                    callback: () => {
                        ++globalThis.instructionCount;
                        this.scene.start("SceneInstructions", {})},
                        loop: false      
                });
            } else {
                this.time.addEvent({
                    delay: 1500,
                    callback: () => {
                        globalThis.data.trial.push(globalThis.data.trial[globalThis.data.trial.length-1]+1)
                        this.scene.start("SceneOffer", {})},
                        loop: false      
                });
            }
        }
    },

    update: function() {
        
    }
});