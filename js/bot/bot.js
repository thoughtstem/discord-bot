const Discord = require('discord.js');
const fs = require('fs');
const client = new Discord.Client();
const { exec } = require("child_process")

client.on('ready', () => {
  console.log(`Logged in as ${client.user.tag}!`);
});

//Use pipes (streams??) instead?
//
//
// 
/*
var readableStream = fs.createReadStream('from-racket.txt');
var writableStream = fs.createWriteStream('to-racket.txt');

readableStream.setEncoding('utf8');

readableStream.on('data', function(chunk) {
    writableStream.write(chunk);
});
*/

var net = require('net')

var server = net.createServer(function (socket) {
  socket.setEncoding("utf8");
  socket.addListener('data', (data) => {
    console.log("DATA")
    var resp = data.toString('utf-8')
    console.log("Got back from Racket: ", resp);
  });
});
server.listen(6969, "localhost");

var racketConn = net.connect(6969, function(x){
  console.log("CONNECT", x)
})

client.on('message', msg => {
	if (msg.content.split("")[0] === config.prefix) {

		var cmd = msg.content.replace(/!/, "")

		var u = msg.author.username
		var d = msg.author.discriminator
		var i = msg.author.id
		var mi = msg.member ? msg.member.id : "NOT_A_MEMBER"

    console.log("Sending to Racket:",
      cmd 
    )

    racketConn.write(cmd.length + " " + cmd)
    //writableStream.write(cmd.length + " " + cmd)

	}
});




function doReply(msg, s){
 var files = extractFiles(s);
 msg.reply(omitFiles(s), {files: files});
}

function omitFiles(s){
 return s.replace(/FILE:\S*/g, "");
}

function extractFiles(s){
  //["data/" + full + ".png"]
  var matches = s.match(/FILE:\S*/g) || []

  return matches.map((x)=>{
     return "bot/data/" + x.replace("FILE:","")
   })
}

const config = require("./config.json");
client.login(config.token);


