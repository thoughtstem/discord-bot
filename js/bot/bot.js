const Discord = require('discord.js');
const fs = require('fs');
const client = new Discord.Client();
const { exec } = require("child_process")

client.on('ready', () => {
  console.log(`Logged in as ${client.user.tag}!`);
});

client.on('message', msg => {

	if (msg.content.split("")[0] === config.prefix) {

		var cmd = msg.content.replace(/!/, "")

		var u = msg.author.username
		var d = msg.author.discriminator
		var i = msg.author.id
		var full = u+"-"+d+"-"+i+".txt"
		fs.writeFile("bot/data/"+full,
			cmd,
			function(err){
				if(err) {
					return console.log(err);
				}

				//TODO: Run in thread or something...
				exec( "racket main.rkt " + "bot/data/" + full
					, (error, stdout, stderr) => {

						//msg.reply(stdout, {files: ["data/" + full + ".png"]});
						
				
						if(!stdout.match("Command not found:"))
    msg.reply(stdout);

						if (error) {
							console.log(`error: ${error.message}`);
							return;
						}
						if (stderr) {
							console.log(`stderr: ${stderr}`);
							return;
						}
						console.log(`stdout: ${stdout}`);
					});

			}
		);


	}
});

const config = require("./config.json");
client.login(config.token);
