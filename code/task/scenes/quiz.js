var SceneQuiz = new Phaser.Class({
    Extends: Phaser.Scene,
    initialize: function() {
        Phaser.Scene.call(this, { "key": "SceneQuiz" });
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

        this.add.text(window.innerWidth*0.5, window.innerHeight*0.075, 
            `Please select all correct statements` , {
                fontSize: 25*globalThis.scaleFactor, color: "#000000", lineSpacing: 5, align: 'left',
        }).setOrigin(0.5, 0.5);

        // submit button
        this.submitButton = this.add.graphics();
        this.submitButton.fillStyle(0xFE8F31, 1);
        this.submitButton.fillRoundedRect((window.innerWidth*0.5)-90*globalThis.scaleFactor, (window.innerHeight*0.75)-30*globalThis.scaleFactor, 180*globalThis.scaleFactor, 60*globalThis.scaleFactor, 10);
        this.submitText = this.add.text(window.innerWidth*0.5, window.innerHeight*0.75,
            "Submit", {
                fontSize: 40*globalThis.scaleFactor, color: "white", 
                backgroundColor: "#FE8F31", padding: {
                    left: 17*globalThis.scaleFactor, right: 17*globalThis.scaleFactor, top: 4*globalThis.scaleFactor, bottom: 4*globalThis.scaleFactor,
                }}).setOrigin(0.5);
        this.submitText.setInteractive({ useHandCursor: true });
            this.submitText.on('pointerdown', () => {
                if(Phaser.Math.IsEven(q1Click)){
                    globalThis.data.choice.push(1)
                } else {
                    globalThis.data.choice.push(0)
                }
                if(Phaser.Math.IsEven(q2Click)){
                    globalThis.data.choice.push(1)
                } else {
                    globalThis.data.choice.push(0)
                }
                if(Phaser.Math.IsEven(q3Click)){
                    globalThis.data.choice.push(1)
                } else {
                    globalThis.data.choice.push(0)
                }
                if(Phaser.Math.IsEven(q4Click)){
                    globalThis.data.choice.push(1)
                } else {
                    globalThis.data.choice.push(0)
                }
                if(Phaser.Math.IsEven(q5Click)){
                    globalThis.data.choice.push(1)
                } else {
                    globalThis.data.choice.push(0)
                }
                if(Phaser.Math.IsEven(q6Click)){
                    globalThis.data.choice.push(1)
                } else {
                    globalThis.data.choice.push(0)
                }
                this.scene.start("SceneFeedback", {});
        });

        this.q1Button = this.add.graphics();
        this.q1Button.lineStyle(3, 0x000000, 1);
        this.q1Button.strokeRoundedRect((window.innerWidth*0.05)-11*globalThis.scaleFactor, (window.innerHeight*0.2)-11*globalThis.scaleFactor, 22*globalThis.scaleFactor, 22*globalThis.scaleFactor, 10);
        this.q1Fill = this.add.graphics();
        this.q1Fill.fillStyle(0xFE8F31, 1);
        this.q1Fill.fillRoundedRect((window.innerWidth*0.05)-9*globalThis.scaleFactor, (window.innerHeight*0.2)-9*globalThis.scaleFactor, 18*globalThis.scaleFactor, 18*globalThis.scaleFactor, 10);
        this.q1Fill.setVisible(false);
        var q1Click = 1;
        this.q1 = this.add.text(window.innerWidth*0.05, window.innerHeight*0.2, " ", {
                fontSize: 10*globalThis.scaleFactor,
                padding: {
                    left: 5*globalThis.scaleFactor, right: 5*globalThis.scaleFactor, top: 3*globalThis.scaleFactor, bottom: 3*globalThis.scaleFactor,
                }}).setOrigin(0.5);
        this.q1.setInteractive({ useHandCursor: true });
            this.q1.on('pointerdown', () => {
                this.updateButton1(++q1Click);
        });
        this.q1 = this.add.text(window.innerWidth*0.075, window.innerHeight*0.2, 
            `I get one point for waiting, when I reject a challenge.` , {
                fontSize: 20*globalThis.scaleFactor, color: "#000000", lineSpacing: 5, align: 'left',
                wordWrap: { width: window.innerWidth*0.45, useAdvancedWrap: true },
        }).setOrigin(0, 0.5);

        this.q2Button = this.add.graphics();
        this.q2Button.lineStyle(3, 0x000000, 1);
        this.q2Button.strokeRoundedRect((window.innerWidth*0.05)-11*globalThis.scaleFactor, (window.innerHeight*0.4)-11*globalThis.scaleFactor, 22*globalThis.scaleFactor, 22*globalThis.scaleFactor, 10);
        this.q2Fill = this.add.graphics();
        this.q2Fill.fillStyle(0xFE8F31, 1);
        this.q2Fill.fillRoundedRect((window.innerWidth*0.05)-9*globalThis.scaleFactor, (window.innerHeight*0.4)-9*globalThis.scaleFactor, 18*globalThis.scaleFactor, 18*globalThis.scaleFactor, 10);
        this.q2Fill.setVisible(false);
        var q2Click = 1;
        this.q2 = this.add.text(window.innerWidth*0.05, window.innerHeight*0.4, " ", {
                fontSize: 10*globalThis.scaleFactor,
                padding: {
                    left: 5*globalThis.scaleFactor, right: 5*globalThis.scaleFactor, top: 3*globalThis.scaleFactor, bottom: 3*globalThis.scaleFactor,
                }}).setOrigin(0.5);
        this.q2.setInteractive({ useHandCursor: true });
            this.q2.on('pointerdown', () => {
                this.updateButton2(++q2Click);
        });
        this.q2 = this.add.text(window.innerWidth*0.075, window.innerHeight*0.4, 
            `I win a challenge once I catch \nthe shrimp, it does not matter \nwhere I am when the time runs out.` , {
                fontSize: 20*globalThis.scaleFactor, color: "#000000", lineSpacing: 5, align: 'left',
                wordWrap: { width: window.innerWidth*0.45, useAdvancedWrap: true },
        }).setOrigin(0, 0.5);

        this.q3Button = this.add.graphics();
        this.q3Button.lineStyle(3, 0x000000, 1);
        this.q3Button.strokeRoundedRect((window.innerWidth*0.05)-11*globalThis.scaleFactor, (window.innerHeight*0.6)-11*globalThis.scaleFactor, 22*globalThis.scaleFactor, 22*globalThis.scaleFactor, 10);
        this.q3Fill = this.add.graphics();
        this.q3Fill.fillStyle(0xFE8F31, 1);
        this.q3Fill.fillRoundedRect((window.innerWidth*0.05)-9*globalThis.scaleFactor, (window.innerHeight*0.6)-9*globalThis.scaleFactor, 18*globalThis.scaleFactor, 18*globalThis.scaleFactor, 10);
        this.q3Fill.setVisible(false);
        var q3Click = 1;
        this.q3 = this.add.text(window.innerWidth*0.05, window.innerHeight*0.6, " ", {
                fontSize: 10*globalThis.scaleFactor,
                padding: {
                    left: 5*globalThis.scaleFactor, right: 5*globalThis.scaleFactor, top: 3*globalThis.scaleFactor, bottom: 3*globalThis.scaleFactor,
                }}).setOrigin(0.5);
        this.q3.setInteractive({ useHandCursor: true });
            this.q3.on('pointerdown', () => {
                this.updateButton3(++q3Click);
        });
        this.q3 = this.add.text(window.innerWidth*0.075, window.innerHeight*0.6, 
            `I should use the same finger as \nI did in the first part of the game.` , {
                fontSize: 20*globalThis.scaleFactor, color: "#000000", lineSpacing: 5, align: 'left',
                wordWrap: { width: window.innerWidth*0.45, useAdvancedWrap: true },
        }).setOrigin(0, 0.5);

        this.q4Button = this.add.graphics();
        this.q4Button.lineStyle(3, 0x000000, 1);
        this.q4Button.strokeRoundedRect((window.innerWidth*0.525)-11*globalThis.scaleFactor, (window.innerHeight*0.2)-11*globalThis.scaleFactor, 22*globalThis.scaleFactor, 22*globalThis.scaleFactor, 10);
        this.q4Fill = this.add.graphics();
        this.q4Fill.fillStyle(0xFE8F31, 1);
        this.q4Fill.fillRoundedRect((window.innerWidth*0.525)-9*globalThis.scaleFactor, (window.innerHeight*0.2)-9*globalThis.scaleFactor, 18*globalThis.scaleFactor, 18*globalThis.scaleFactor, 10);
        this.q4Fill.setVisible(false);
        var q4Click = 1;
        this.q4 = this.add.text(window.innerWidth*0.525, window.innerHeight*0.2, " ", {
                fontSize: 10*globalThis.scaleFactor,
                padding: {
                    left: 5*globalThis.scaleFactor, right: 5*globalThis.scaleFactor, top: 3*globalThis.scaleFactor, bottom: 3*globalThis.scaleFactor,
                }}).setOrigin(0.5);
        this.q4.setInteractive({ useHandCursor: true });
            this.q4.on('pointerdown', () => {
                this.updateButton4(++q4Click);
        });
        this.q4 = this.add.text(window.innerWidth*0.55, window.innerHeight*0.2, 
            `I should consider both effort and \nreward when making decisions.` , {
                fontSize: 20*globalThis.scaleFactor, color: "#000000", lineSpacing: 5, align: 'left',
                wordWrap: { width: window.innerWidth*0.45, useAdvancedWrap: true },
        }).setOrigin(0, 0.5);

        this.q5Button = this.add.graphics();
        this.q5Button.lineStyle(3, 0x000000, 1);
        this.q5Button.strokeRoundedRect((window.innerWidth*0.525)-11*globalThis.scaleFactor, (window.innerHeight*0.4)-11*globalThis.scaleFactor, 22*globalThis.scaleFactor, 22*globalThis.scaleFactor, 10);
        this.q5Fill = this.add.graphics();
        this.q5Fill.fillStyle(0xFE8F31, 1);
        this.q5Fill.fillRoundedRect((window.innerWidth*0.525)-9*globalThis.scaleFactor, (window.innerHeight*0.4)-9*globalThis.scaleFactor, 18*globalThis.scaleFactor, 18*globalThis.scaleFactor, 10);
        this.q5Fill.setVisible(false);
        var q5Click = 1;
        this.q5 = this.add.text(window.innerWidth*0.525, window.innerHeight*0.4, " ", {
                fontSize: 10*globalThis.scaleFactor,
                padding: {
                    left: 5*globalThis.scaleFactor, right: 5*globalThis.scaleFactor, top: 3*globalThis.scaleFactor, bottom: 3*globalThis.scaleFactor,
                }}).setOrigin(0.5);
        this.q5.setInteractive({ useHandCursor: true });
            this.q5.on('pointerdown', () => {
                this.updateButton5(++q5Click);
        });
        this.q5 = this.add.text(window.innerWidth*0.55, window.innerHeight*0.4, 
            `I should use the middle finger \nof my dominant hand.` , {
                fontSize: 20*globalThis.scaleFactor, color: "#000000", lineSpacing: 5, align: 'left',
                wordWrap: { width: window.innerWidth*0.45, useAdvancedWrap: true },
        }).setOrigin(0, 0.5);

        this.q6Button = this.add.graphics();
        this.q6Button.lineStyle(3, 0x000000, 1);
        this.q6Button.strokeRoundedRect((window.innerWidth*0.525)-11*globalThis.scaleFactor, (window.innerHeight*0.6)-11*globalThis.scaleFactor, 22*globalThis.scaleFactor, 22*globalThis.scaleFactor, 10);
        this.q6Fill = this.add.graphics();
        this.q6Fill.fillStyle(0xFE8F31, 1);
        this.q6Fill.fillRoundedRect((window.innerWidth*0.525)-9*globalThis.scaleFactor, (window.innerHeight*0.6)-9*globalThis.scaleFactor, 18*globalThis.scaleFactor, 18*globalThis.scaleFactor, 10);
        this.q6Fill.setVisible(false);
        var q6Click = 1;
        this.q6 = this.add.text(window.innerWidth*0.525, window.innerHeight*0.6, " ", {
                fontSize: 10*globalThis.scaleFactor,
                padding: {
                    left: 5*globalThis.scaleFactor, right: 5*globalThis.scaleFactor, top: 3*globalThis.scaleFactor, bottom: 3*globalThis.scaleFactor,
                }}).setOrigin(0.5);
        this.q6.setInteractive({ useHandCursor: true });
            this.q6.on('pointerdown', () => {
                this.updateButton6(++q6Click);
        });
        this.q6 = this.add.text(window.innerWidth*0.55, window.innerHeight*0.6, 
            `I should only consider the effort \nlevel when making decisions.` , {
                fontSize: 20*globalThis.scaleFactor, color: "#000000", lineSpacing: 5, align: 'left',
                wordWrap: { width: window.innerWidth*0.45, useAdvancedWrap: true },
        }).setOrigin(0, 0.5);

        //this.scene.start("SceneFeedback", {});
    },

    updateButton1: function(q1Click) {
        if(Phaser.Math.IsEven(q1Click)){
            this.q1Fill.setVisible(true)
        } else {
            this.q1Fill.setVisible(false)
        }
    },

    updateButton2: function(q2Click) {
        if(Phaser.Math.IsEven(q2Click)){
            this.q2Fill.setVisible(true)
        } else {
            this.q2Fill.setVisible(false)
        }
    },

    updateButton3: function(q3Click) {
        if(Phaser.Math.IsEven(q3Click)){
            this.q3Fill.setVisible(true)
        } else {
            this.q3Fill.setVisible(false)
        }
    },

    updateButton4: function(q4Click) {
        if(Phaser.Math.IsEven(q4Click)){
            this.q4Fill.setVisible(true)
        } else {
            this.q4Fill.setVisible(false)
        }
    },

    updateButton5: function(q5Click) {
        if(Phaser.Math.IsEven(q5Click)){
            this.q5Fill.setVisible(true)
        } else {
            this.q5Fill.setVisible(false)
        }
    },

    updateButton6: function(q6Click) {
        if(Phaser.Math.IsEven(q6Click)){
            this.q6Fill.setVisible(true)
        } else {
            this.q6Fill.setVisible(false)
        }
    },
    update: function() {

    }
});