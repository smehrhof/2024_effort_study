var SceneInstructions = new Phaser.Class({
    Extends: Phaser.Scene,
    initialize: function() {
        Phaser.Scene.call(this, { "key": "SceneInstructions" });
    },

    init: function() {
    },

    preload: function() {
        // background images
        this.load.image('water', 'assets/undersea.jpeg');
        this.load.image('seabed', 'assets/seabed.png');
        // avatars
        this.load.spritesheet('squid', 'assets/squids.png',
            { frameWidth: 84, frameHeight: 97 });
        this.load.spritesheet('shrimp', 'assets/shrimp.png',
            { frameWidth: 66, frameHeight: 71 });
        this.load.image('star', 'assets/star.png',
            { frameWidth: 38, frameHeight: 37 });
        //effort and reward images
        this.load.image('effort1', 'assets/effort_1.png');
        this.load.image('effort2', 'assets/effort_2.png');
        this.load.image('effort3', 'assets/effort_3.png');
        this.load.image('effort4', 'assets/effort_4.png');
        this.load.image('reward1', 'assets/reward_1.png');
        this.load.image('reward2', 'assets/reward_2.png');
        this.load.image('reward3', 'assets/reward_3.png');
        this.load.image('reward4', 'assets/reward_4.png');
        this.load.image('winReward1', 'assets/winReward_1.png');
        this.load.image('winReward2', 'assets/winReward_2.png');
        this.load.image('winReward3', 'assets/winReward_3.png');
        this.load.image('winReward4', 'assets/winReward_4.png');
    },
    
    create: function() {
        // add background and scale to browser window 
        this.background = this.add.image(window.innerWidth*0.5, window.innerHeight*0.5, 'water');
        var scaleX = window.innerWidth / this.background.width;
        var scaleY = window.innerHeight / this.background.height;
        this.background.setScale(scaleX, scaleY);
        this.seabed = this.add.image(window.innerWidth*0.5, window.innerHeight, 'seabed').setOrigin(0.5,1);
        this.seabed.setScale(scaleX, scaleY);

        // text to adjust
        this.welcomeText = this.add.text(window.innerWidth*0.5, window.innerHeight*0.3, 
            ``, {
            fontSize: 60*globalThis.scaleFactor, color: "#000000", lineSpacing: 5, align: 'center',
        }).setOrigin(0.5, 0);

        this.instructionText = this.add.text(window.innerWidth*0.5, window.innerHeight*0.5, 
            ``, {
            fontSize: 25*globalThis.scaleFactor, color: "#000000", lineSpacing: 5,
            wordWrap: { width: window.innerWidth*0.65, useAdvancedWrap: true },
        }).setOrigin(0.5, 0.5);

        this.blocksText = this.add.text(window.innerWidth*0.5, window.innerHeight*0.45, 
            ``, {
            fontSize: 30*globalThis.scaleFactor, color: "#000000", lineSpacing: 5,
        }).setOrigin(0.5, 0.5);

        // are you ready? text
        this.readyText = this.add.text(window.innerWidth*0.5, window.innerHeight*0.4,
            ``, {
            fontSize: 50*globalThis.scaleFactor, color: "#000000", lineSpacing: 5, align: 'center',
        }).setOrigin(0.5, 0.5);

        // start with first text
        this.time.addEvent({delay: 0,
            callback: () => {this.updateText();},
            loop: false
        });

        window.addEventListener('resize', () => {
            this.updateSize();
        });

        // first button
        this.firstButton = this.add.graphics();
        this.firstButton.fillStyle(0xFE8F31, 1);
        this.firstButton.fillRoundedRect((window.innerWidth*0.5)-150*globalThis.scaleFactor, (window.innerHeight*0.75)-30*globalThis.scaleFactor, 300*globalThis.scaleFactor, 60*globalThis.scaleFactor, 10);
        this.firstText = this.add.text(window.innerWidth*0.5, window.innerHeight*0.75,
            "Get Started", {
                fontSize: 35*globalThis.scaleFactor, color: "white", 
                backgroundColor: "#FE8F31", padding: {
                    left: 20*globalThis.scaleFactor, right: 20*globalThis.scaleFactor, top: 4*globalThis.scaleFactor, bottom: 4*globalThis.scaleFactor,
                }}).setOrigin(0.5);
        this.firstText.setInteractive({ useHandCursor: true });
            this.firstText.on('pointerdown', () => {
                this.updateText(++globalThis.instructionCount)
        });

        // centre button
        this.centreButton = this.add.graphics();
        this.centreButton.fillStyle(0xFE8F31, 1);
        this.centreButton.fillRoundedRect((window.innerWidth*0.5)-90*globalThis.scaleFactor, (window.innerHeight*0.75)-30*globalThis.scaleFactor, 180*globalThis.scaleFactor, 60*globalThis.scaleFactor, 10);
        this.centreText = this.add.text(window.innerWidth*0.5, window.innerHeight*0.75,
            "Play", {
                fontSize: 40*globalThis.scaleFactor, color: "white", 
                backgroundColor: "#FE8F31", padding: {
                    left: 40*globalThis.scaleFactor, right: 40*globalThis.scaleFactor, top: 4*globalThis.scaleFactor, bottom: 4*globalThis.scaleFactor,
                }}).setOrigin(0.5);
        this.centreText.setInteractive({ useHandCursor: true });
            this.centreText.on('pointerdown', () => {
                this.updateText(++globalThis.instructionCount)
        });

        // next button
        this.nextButton = this.add.graphics();
        this.nextButton.fillStyle(0xFE8F31, 1);
        this.nextButton.fillRoundedRect((window.innerWidth*0.625)-90*globalThis.scaleFactor, (window.innerHeight*0.75)-30*globalThis.scaleFactor, 180*globalThis.scaleFactor, 60*globalThis.scaleFactor, 10);
        this.nextText = this.add.text(window.innerWidth*0.625, window.innerHeight*0.75,
            "Next", {
                fontSize: 40*globalThis.scaleFactor, color: "white", 
                backgroundColor: "#FE8F31", padding: {
                    left: 40*globalThis.scaleFactor, right: 40*globalThis.scaleFactor, top: 4*globalThis.scaleFactor, bottom: 4*globalThis.scaleFactor,
                }}).setOrigin(0.5);

        this.nextText.setInteractive({ useHandCursor: true });
            this.nextText.on('pointerdown', () => {
                this.updateText(++globalThis.instructionCount)
        });

        // back button
        this.backButton = this.add.graphics()
        this.backButton.fillStyle(0xFE8F31, 1);
        this.backButton.fillRoundedRect((window.innerWidth*0.375)-90*globalThis.scaleFactor, (window.innerHeight*0.75)-30*globalThis.scaleFactor, 180*globalThis.scaleFactor, 60*globalThis.scaleFactor, 10);
        this.backText = this.add.text(window.innerWidth*0.375, window.innerHeight*0.75,
            "Back", {
                fontSize: 40*globalThis.scaleFactor, color: "white", 
                backgroundColor: "#FE8F31", padding: {
                    left: 40*globalThis.scaleFactor, right: 40*globalThis.scaleFactor, top: 4*globalThis.scaleFactor, bottom: 4*globalThis.scaleFactor,
                }}).setOrigin(0.5);

        this.backText.setInteractive({ useHandCursor: true });
            this.backText.on('pointerdown', () => {
                this.updateText(--globalThis.instructionCount)
        });
 
    },

    updateText: function() {
        // next text
        if(globalThis.instructionCount == 0){
            this.welcomeText.setVisible(true);
            this.welcomeText.setText(`Welcome!`)
            this.instructionText.setText(`Thanks for playing`);
            this.instructionText.setFontSize(35*globalThis.scaleFactor);
            this.instructionText.setY(window.innerHeight*0.5) 

            this.firstText.setVisible(true);
            this.firstButton.setVisible(true);
            this.centreText.setVisible(false);
            this.centreButton.setVisible(false);
            this.nextText.setVisible(false);
            this.nextButton.setVisible(false);
            this.backText.setVisible(false);
            this.backButton.setVisible(false);

        // next text
        } else if(globalThis.instructionCount == 1){
            this.welcomeText.setVisible(false);

            this.instructionText.setText(`In the first part of the game, your goal is to collect points by filling up the water with stars. Place your computer mouse anywhere in the underwater scene and click as fast as you can, using the finger you would usually use to make mouse clicks.`);
            this.instructionText.setFontSize(25*globalThis.scaleFactor);
            this.instructionText.setY(window.innerHeight*0.375) 

            this.nextText.setText(`Next`);
            this.nextText.setPadding(40*globalThis.scaleFactor, 4*globalThis.scaleFactor, 40*globalThis.scaleFactor, 4*globalThis.scaleFactor);

            this.firstText.setVisible(false);
            this.firstButton.setVisible(false);
            this.centreText.setVisible(false);
            this.centreButton.setVisible(false);
            this.nextText.setVisible(true);
            this.nextButton.setVisible(true);
            this.backText.setVisible(true);
            this.backButton.setVisible(true);

        // next text
        } else if(globalThis.instructionCount == 2){
            this.instructionText.setText(`At the top of the screen, you will see a time bar indicating how much time is left. As you collect points, you will see stars filling up the water. Do not click on the stars to collect them, just place your mouse anywhere and start to click. \n\nTime is short, so start clicking right away and do not take breaks!`);
            
            this.nextText.setText(`Start`);
            this.nextText.setPadding(28*globalThis.scaleFactor, 4*globalThis.scaleFactor, 28*globalThis.scaleFactor, 4*globalThis.scaleFactor);
        
        // ready to go?
        } else if(globalThis.instructionCount == 3){

            this.instructionText.setText(``);
            this.readyText.setText(`Ready to go?\nPress Play!`);

            this.firstText.setVisible(false);
            this.firstButton.setVisible(false);
            this.centreText.setVisible(true);
            this.centreButton.setVisible(true);
            this.nextText.setVisible(false);
            this.nextButton.setVisible(false);
            this.backText.setVisible(false);
            this.backButton.setVisible(false);

            this.centreText.setText(`Play`);
    
        // start next phase
        } else if(globalThis.instructionCount == 4){
            for(var i = 0; i < 3; i++){
                globalThis.data.phase.push('calibration')
                globalThis.data.trial.push(i+1)
                globalThis.data.trialType.push(999)
                globalThis.data.offerEffort.push(999)
                globalThis.data.offerReward.push(999)
                globalThis.data.choice.push(999)
                globalThis.data.rt.push(999)
                globalThis.data.goalClicks.push(999)
                globalThis.data.points.push(0)
            }
            this.scene.start("SceneCountdown", {});

        } else if(globalThis.instructionCount == 5){
            
            // delay in making the button clickable to avoid accidental clicking
            this.centreText.removeInteractive();
            this.time.addEvent({
                delay: 500,
                repeat: 3,
                callback: () => {
                    this.centreText.setInteractive({ useHandCursor: true });
                }
            });

            this.instructionText.setText(``);
            this.readyText.setText(`You collected ` + globalThis.data.clicks[globalThis.data.clicks.length-1] + ` stars!\nCan you get even more?\nPress Play!`);
            this.readyText.setFontSize(40*globalThis.scaleFactor);

            this.firstText.setVisible(false);
            this.firstButton.setVisible(false);
            this.centreText.setVisible(true);
            this.centreButton.setVisible(true);
            this.nextText.setVisible(false);
            this.nextButton.setVisible(false);
            this.backText.setVisible(false);
            this.backButton.setVisible(false);

            this.centreText.setText(`Play`);

        }  else if(globalThis.instructionCount == 6){
            this.scene.start("SceneCountdown", {});

        } else if(globalThis.instructionCount == 7){

            // delay in making the button clickable to avoid accidental clicking
            this.centreText.removeInteractive();
            this.time.addEvent({
                delay: 500,
                repeat: 3,
                callback: () => {
                    this.centreText.setInteractive({ useHandCursor: true });
                }
            });

            this.instructionText.setText(``);
            this.readyText.setText(`You collected ` + globalThis.data.clicks[globalThis.data.clicks.length-1] + ` stars!\nCan you beat your record?\nPress Play!`);
            this.readyText.setFontSize(40*globalThis.scaleFactor);

            this.firstText.setVisible(false);
            this.firstButton.setVisible(false);
            this.centreText.setVisible(true);
            this.centreButton.setVisible(true);
            this.nextText.setVisible(false);
            this.nextButton.setVisible(false);
            this.backText.setVisible(false);
            this.backButton.setVisible(false);

            this.centreText.setText(`Play`);

        }  else if(globalThis.instructionCount == 8){
            this.scene.start("SceneCountdown", {});

        } else if(globalThis.instructionCount == 9){
            //jatos.submitResultData(globalThis.data);

            globalThis.clickSpeed = Phaser.Math.Average(globalThis.data.clicks.slice(1,3))/10;

            this.instructionText.setText(`In the next part of the game, you can win points by helping an octopus catch shrimp. Use the same finger you used in the previous part of the game to click the mouse and let the octopus swim towards the shrimp.`);
            this.instructionText.setY(window.innerHeight*0.375) 

            // delay in making the button clickable to avoid accidental clicking
            this.centreText.removeInteractive();
            this.time.addEvent({
                delay: 500,
                repeat: 3,
                callback: () => {
                    this.centreText.setInteractive({ useHandCursor: true });
                }
            });
            this.centreText.setText(`Next`);
            this.nextText.setPadding(40*globalThis.scaleFactor, 4*globalThis.scaleFactor, 40*globalThis.scaleFactor, 4*globalThis.scaleFactor);

            this.firstText.setVisible(false);
            this.firstButton.setVisible(false);
            this.centreText.setVisible(true);
            this.centreButton.setVisible(true);
            this.nextText.setVisible(false);
            this.nextButton.setVisible(false);
            this.backText.setVisible(false);
            this.backButton.setVisible(false);
        
        } else if(globalThis.instructionCount == 10){
            this.instructionText.setText(`A text at the top of the screen will indicate whether you are close enough to the shrimp to catch it. You then have to keep up with its speed as you will only win points if the octopus catches up with the shrimp, or is further along when time runs out.`);
            this.instructionText.setY(window.innerHeight*0.375) 

            this.nextText.setText(`Next`);
            this.nextText.setPadding(40*globalThis.scaleFactor, 4*globalThis.scaleFactor, 40*globalThis.scaleFactor, 4*globalThis.scaleFactor);

            this.firstText.setVisible(false);
            this.firstButton.setVisible(false);
            this.centreText.setVisible(false);
            this.centreButton.setVisible(false);
            this.nextText.setVisible(true);
            this.nextButton.setVisible(true);
            this.backText.setVisible(true);
            this.backButton.setVisible(true);

        // next text
        } else if(globalThis.instructionCount == 11){
            this.instructionText.setText(`Catching the shrimp will require more or less effort, depending on how fast that shrimp is, and on how long you have to chase it for. This effort amount will be indicated as one of four levels, which you will be told in advance. To get a feeling for each level, you will now practice them.`);
            this.instructionText.setY(window.innerHeight*0.375) 
            
            this.nextText.setText(`Start`);
            this.nextText.setPadding(28*globalThis.scaleFactor, 4*globalThis.scaleFactor, 28*globalThis.scaleFactor, 4*globalThis.scaleFactor);

        // start practice phase 
        } else if(globalThis.instructionCount == 12){
            for(var i = 0; i < 4; i++){
                globalThis.data.phase.push('practice')
                globalThis.data.trialType.push(999)
                globalThis.data.offerReward.push(999)
                globalThis.data.choice.push(999)
                globalThis.data.rt.push(999)
                globalThis.data.points.push(0)
            } 
            globalThis.data.trial.push(1)     
            this.scene.start("SceneOffer", {});

            globalThis.quizTries = 0;
        // next text
        } else if(globalThis.instructionCount == 13){
            //jatos.submitResultData(globalThis.data);

            this.instructionText.setText(`We will now start the main part of the game. You will be presented with a range of challenges consisting of an effort level and a reward. The reward is points you collect throughout the game.`);
            this.instructionText.setY(window.innerHeight*0.375) 

            this.firstText.setVisible(false);
            this.firstButton.setVisible(false);
            this.centreText.setVisible(true);
            this.centreButton.setVisible(true);
            this.nextText.setVisible(false);
            this.nextButton.setVisible(false);
            this.backText.setVisible(false);
            this.backButton.setVisible(false);

            this.centreText.setText(`Next`);
            this.centreText.setPadding(40*globalThis.scaleFactor, 4*globalThis.scaleFactor, 40*globalThis.scaleFactor, 4*globalThis.scaleFactor);

        // next text
        } else if(globalThis.instructionCount == 14){
            this.instructionText.setText(`If you accept the challenge, you have to catch the shrimp (by clicking, like you practiced) to win the points. If you reject, you do not have to do anything but wait for the same amount of time - you still get one point for waiting.`);
            this.instructionText.setY(window.innerHeight*0.375) 

            this.firstText.setVisible(false);
            this.firstButton.setVisible(false);
            this.centreText.setVisible(false);
            this.centreButton.setVisible(false);
            this.nextText.setVisible(true);
            this.nextButton.setVisible(true);
            this.backText.setVisible(true);
            this.backButton.setVisible(true);

            this.nextText.setText(`Next`);
            this.nextText.setPadding(40*globalThis.scaleFactor, 4*globalThis.scaleFactor, 40*globalThis.scaleFactor, 4*globalThis.scaleFactor);

        // next text
        } else if(globalThis.instructionCount == 15){
            this.instructionText.setText(`When making the decision to accept or reject a challenge, make sure you take both the required effort and the offered reward into account. You will be given the chance to take breaks between blocks of challenges. Please do not take any other breaks.`);
            this.instructionText.setY(window.innerHeight*0.375) 

            this.nextText.setText(`Next`);
            this.nextText.setPadding(28*globalThis.scaleFactor, 4*globalThis.scaleFactor, 28*globalThis.scaleFactor, 4*globalThis.scaleFactor);
        
        } else if(globalThis.instructionCount == 16){
            this.instructionText.setText(`Remember, you can increase your chances of winning the bonus payment of £10! Importantly, this is not linked to the points you win, but rather how well you follow the instructions. Make your decisions to accept or reject offers carefully, considering the offered effort and reward, and do not take breaks unless indicated.`);
            this.instructionText.setY(window.innerHeight*0.375) 

            this.nextText.setText(`Next`);
            this.nextText.setPadding(28*globalThis.scaleFactor, 4*globalThis.scaleFactor, 28*globalThis.scaleFactor, 4*globalThis.scaleFactor);

        } else if(globalThis.instructionCount == 17){
            this.instructionText.setText(`Before starting the game we will test if you understood the game instructions. On the next page, you will see six statements about the game. Please select all that are correct.`);
            this.instructionText.setY(window.innerHeight*0.375) 

            this.nextText.setText(`Start`);
            this.nextText.setPadding(28*globalThis.scaleFactor, 4*globalThis.scaleFactor, 28*globalThis.scaleFactor, 4*globalThis.scaleFactor);

        } else if(globalThis.instructionCount == 18){
            globalThis.quizTries++
            for(var i = 0; i < 6; i++){
                globalThis.data.phase.push('quiz')
                globalThis.data.trialType.push(i)
                globalThis.data.offerEffort.push(999)
                globalThis.data.offerReward.push(999)
                globalThis.data.rt.push(999)
                globalThis.data.clicks.push(999)
                globalThis.data.goalClicks.push(999)
                globalThis.data.points.push(0)
                globalThis.data.trial.push(globalThis.quizTries)
            }
            this.scene.start("SceneQuiz", {});

        // start game phase
        // block 1 
        } else if(globalThis.instructionCount == 19){
            for(var i = 0; i < 16; i++){
                globalThis.data.phase.push('game')
            }
            globalThis.data.trial.push(1)    
            this.scene.start("SceneOffer", {});

        } else if(globalThis.instructionCount == 20){
            //jatos.submitResultData(globalThis.data);

            this.welcomeText.setVisible(true);
            this.welcomeText.setFontSize(60);
            this.welcomeText.setText(`You can take a break now!`);
            this.blocksText.setText('1/4 challenge blocks completed')
            this.instructionText.setY(window.innerHeight*0.6) 
            this.instructionText.setText(`Press Play to continue`);

            this.firstText.setVisible(false);
            this.firstButton.setVisible(false);
            this.centreText.setVisible(true);
            this.centreButton.setVisible(true);
            this.nextText.setVisible(false);
            this.nextButton.setVisible(false);
            this.backText.setVisible(false);
            this.backButton.setVisible(false);

            this.centreText.setText(`Play`);
            this.centreText.setPadding(40*globalThis.scaleFactor, 4*globalThis.scaleFactor, 40*globalThis.scaleFactor, 4*globalThis.scaleFactor);

            globalThis.data.phase.push('break');
            globalThis.data.trial.push(17);
            globalThis.data.trialType.push(999)
            globalThis.data.offerEffort.push(999)
            globalThis.data.offerReward.push(999)
            globalThis.data.choice.push(999)
            globalThis.data.clicks.push(999)
            globalThis.data.goalClicks.push(999)
            globalThis.data.points.push(globalThis.data.points[globalThis.data.points.length-1])

            // stop time
            start = new Date();
            this.startTime = start.getTime();

        // block 2
        } else if(globalThis.instructionCount == 21){
            //jatos.submitResultData(globalThis.data);

            end = new Date();
            this.endTime = end.getTime();
            globalThis.data.rt.push(this.endTime - this.startTime);

            for(var i = 0; i < 16; i++){
                globalThis.data.phase.push('game')
            }
            this.scene.start("SceneOffer", {});

        } else if(globalThis.instructionCount == 22){
            //jatos.submitResultData(globalThis.data);

            this.welcomeText.setVisible(true);
            this.welcomeText.setFontSize(60);
            this.welcomeText.setText(`You can take a break now!`);
            this.blocksText.setText('2/4 challenge blocks completed')
            this.instructionText.setY(window.innerHeight*0.6) 
            this.instructionText.setText(`Press Play to continue`);

            this.firstText.setVisible(false);
            this.firstButton.setVisible(false);
            this.centreText.setVisible(true);
            this.centreButton.setVisible(true);
            this.nextText.setVisible(false);
            this.nextButton.setVisible(false);
            this.backText.setVisible(false);
            this.backButton.setVisible(false);

            this.centreText.setText(`Play`);
            this.centreText.setPadding(40*globalThis.scaleFactor, 4*globalThis.scaleFactor, 40*globalThis.scaleFactor, 4*globalThis.scaleFactor);

            globalThis.data.phase.push('break');
            globalThis.data.trial.push(33);
            globalThis.data.trialType.push(999)
            globalThis.data.offerEffort.push(999)
            globalThis.data.offerReward.push(999)
            globalThis.data.choice.push(999)
            globalThis.data.clicks.push(999)
            globalThis.data.goalClicks.push(999)
            globalThis.data.points.push(globalThis.data.points[globalThis.data.points.length-1])

            // stop time
            start = new Date();
            this.startTime = start.getTime();

        // block 3
        } else if(globalThis.instructionCount == 23){
            end = new Date();
            this.endTime = end.getTime();
            globalThis.data.rt.push(this.endTime - this.startTime);

            for(var i = 0; i < 16; i++){
                globalThis.data.phase.push('game')
            }
            this.scene.start("SceneOffer", {});

        } else if(globalThis.instructionCount == 24){
            //jatos.submitResultData(globalThis.data);

            this.welcomeText.setVisible(true);
            this.welcomeText.setFontSize(60);
            this.welcomeText.setText(`You can take a break now!`);
            this.blocksText.setText('3/4 challenge blocks completed')
            this.instructionText.setY(window.innerHeight*0.6) 
            this.instructionText.setText(`Press Play to continue`);

            this.firstText.setVisible(false);
            this.firstButton.setVisible(false);
            this.centreText.setVisible(true);
            this.centreButton.setVisible(true);
            this.nextText.setVisible(false);
            this.nextButton.setVisible(false);
            this.backText.setVisible(false);
            this.backButton.setVisible(false);

            this.centreText.setText(`Play`);
            this.centreText.setPadding(40*globalThis.scaleFactor, 4*globalThis.scaleFactor, 40*globalThis.scaleFactor, 4*globalThis.scaleFactor);

            globalThis.data.phase.push('break');
            globalThis.data.trial.push(49);
            globalThis.data.trialType.push(999)
            globalThis.data.offerEffort.push(999)
            globalThis.data.offerReward.push(999)
            globalThis.data.choice.push(999)
            globalThis.data.clicks.push(999)
            globalThis.data.goalClicks.push(999)
            globalThis.data.points.push(globalThis.data.points[globalThis.data.points.length-1])

            // stop time
            start = new Date();
            this.startTime = start.getTime();

        // block 4
        } else if(globalThis.instructionCount == 25){
            end = new Date();
            this.endTime = end.getTime();
            globalThis.data.rt.push(this.endTime - this.startTime);
            
            for(var i = 0; i < 16; i++){
                globalThis.data.phase.push('game')
            }
            this.scene.start("SceneOffer", {});

        } else if(globalThis.instructionCount == 26){
            //jatos.submitResultData(globalThis.data);
            
            this.welcomeText.setFontSize(70);
            this.welcomeText.setText(`Well done!`);
            this.instructionText.setFontSize(50);
            this.instructionText.setY(window.innerHeight*0.5) 
            this.instructionText.setText(`You won a total of ` + globalThis.data.points[globalThis.data.points.length-1] + ` points`);

            this.firstText.setVisible(false);
            this.firstButton.setVisible(false);
            this.centreText.setVisible(true);
            this.centreButton.setVisible(true);
            this.nextText.setVisible(false);
            this.nextButton.setVisible(false);
            this.backText.setVisible(false);
            this.backButton.setVisible(false);
            this.blocksText.setVisible(false);

            this.centreText.setText(`Next`);
            this.centreText.setPadding(40*globalThis.scaleFactor, 4*globalThis.scaleFactor, 40*globalThis.scaleFactor, 4*globalThis.scaleFactor);

        } else if(globalThis.instructionCount == 27){
            this.welcomeText.setText(``);
            this.instructionText.setText(`Before you finish, you just need to briefly repeat the very first challenge of this game: Click as fast as you can using the same finger you used so far, to collect stars.`);
            this.instructionText.setFontSize(25*globalThis.scaleFactor);
            this.instructionText.setY(window.innerHeight*0.375);

            this.firstText.setVisible(false);
            this.firstButton.setVisible(false);
            this.centreText.setVisible(false);
            this.centreButton.setVisible(false);
            this.nextText.setVisible(true);
            this.nextButton.setVisible(true);
            this.backText.setVisible(true);
            this.backButton.setVisible(true);
            this.blocksText.setVisible(false);

            this.nextText.setText(`Start`);
            this.nextText.setPadding(28*globalThis.scaleFactor, 4*globalThis.scaleFactor, 28*globalThis.scaleFactor, 4*globalThis.scaleFactor);

        } else if(globalThis.instructionCount == 28){
            this.instructionText.setText(``);
            this.readyText.setText(`Ready to go?\nPress play!`);
            this.readyText.setFontSize(50*globalThis.scaleFactor);


            this.firstText.setVisible(false);
            this.firstButton.setVisible(false);
            this.centreText.setVisible(true);
            this.centreButton.setVisible(true);
            this.nextText.setVisible(false);
            this.nextButton.setVisible(false);
            this.backText.setVisible(false);
            this.backButton.setVisible(false);
            this.blocksText.setVisible(false);

            this.centreText.setText(`Play`);

        } else if(globalThis.instructionCount == 29){
            globalThis.data.phase.push('calibration')
            globalThis.data.offerEffort.push(999)
            globalThis.data.offerReward.push(999)
            globalThis.data.choice.push(999)
            globalThis.data.rt.push(999)
            globalThis.data.goalClicks.push(999)
            globalThis.data.trialType.push(999)
            globalThis.data.points.push(globalThis.data.points[globalThis.data.points.length-1])
            
            globalThis.data.trial.push(1) 
            this.scene.start("SceneCountdown", {});

        } else if(globalThis.instructionCount == 30){
            this.welcomeText.setFontSize(70);
            this.welcomeText.setText(`That is it!\nThank you for playing`);
            this.welcomeText.setY(window.innerHeight*0.25) 
            this.instructionText.setText(`Please press End to proceed to the final part of the experiment`);
            this.instructionText.setStyle({
                align: 'center',
            });
            this.instructionText.setY(window.innerHeight*0.6) 

            this.firstText.setVisible(false);
            this.firstButton.setVisible(false);
            this.centreText.setVisible(true);
            this.centreButton.setVisible(true);
            this.nextText.setVisible(false);
            this.nextButton.setVisible(false);
            this.backText.setVisible(false);
            this.backButton.setVisible(false);
            this.blocksText.setVisible(false);

            this.centreText.setText(`End`);
            this.centreText.setPadding(45*globalThis.scaleFactor, 4*globalThis.scaleFactor, 45*globalThis.scaleFactor, 4*globalThis.scaleFactor);

        } else if(globalThis.instructionCount == 31){

            globalThis.data.date.push(new Date());
            jatos.submitResultData(globalThis.data, jatos.startNextComponent);

        }
    },

    updateSize: function() {
        var scaleX = window.innerWidth / this.background.width;
        var scaleY = window.innerHeight / this.background.height;
        this.background.setScale(scaleX, scaleY);
        this.background.setPosition(window.innerWidth*0.5, window.innerHeight*0.5);
        this.seabed.setScale(scaleX, scaleY);
        this.seabed.setPosition(window.innerWidth*0.5, window.innerHeight);
        this.welcomeText.setPosition(window.innerWidth*0.5, window.innerHeight*0.3);
        this.instructionText.setPosition(window.innerWidth*0.5, window.innerHeight*0.5);
        this.blocksText.setPosition(window.innerWidth*0.5, window.innerHeight*0.45);
        this.readyText.setPosition(window.innerWidth*0.5, window.innerHeight*0.4);
        this.firstButton.setPosition((window.innerWidth*0.5)-150*globalThis.scaleFactor, (window.innerHeight*0.75)-30*globalThis.scaleFactor);
        this.centreButton.setPosition((window.innerWidth*0.5)-90*globalThis.scaleFactor, (window.innerHeight*0.75)-30*globalThis.scaleFactor);
        this.nextButton.setPosition((window.innerWidth*0.625)-90*globalThis.scaleFactor, (window.innerHeight*0.75)-30*globalThis.scaleFactor);
        this.backButton.setPosition((window.innerWidth*0.375)-90*globalThis.scaleFactor, (window.innerHeight*0.75)-30*globalThis.scaleFactor);
        this.firstText.setPosition(window.innerWidth*0.5, window.innerHeight*0.75);
        this.centreText.setPosition(window.innerWidth*0.5, window.innerHeight*0.75);
        this.nextText.setPosition(window.innerWidth*0.625, window.innerHeight*0.75);
        this.backText.setPosition(window.innerWidth*0.375, window.innerHeight*0.75);

    },

    update: function() {
        
    }
});


