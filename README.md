# node-manager [![Circle CI](https://circleci.com/gh/plow-technologies/node-manager.png?style=shield)](https://circleci.com/gh/plow-technologies/node-manager)

Node manager is a webserver with background processes designed help monitor node health.
Simple commands let the user control lots of different types of processes. 

* node-managers talk to each other to provide redundant monitoring of processes.

* everything in node-manager is designed to be simple and reliable.
  * Disk IO is simple acid state
  * All messages are JSON


## Deployment

Pushing to the master branch causes circleci to build the new binary for node-manager, if it passes, it then sends it to staging, restarts the process, and makes a copy with the date to the release history folder on that server.

Pushing to the production branch causes circleci to build the new binary for node-manager, if it passes, it then sends it to production, restarts the process and makes a copy with the date to the release history folder on that server.

## Using to configure Nodes

### Adding a Configuration

To add a new configuration to serve to nodes...

```
$> post "http://some.lame.nodemanager.com/configure/add" (toJSON (object ["alarm-state-config" .= object  [ ( "tag" .= 2), ("src" .= (object ["almKeySrc" .= (object [ "unSText" .=  "onping.plowtech.net"])])),  ("host" .= "www.stupidurl.com"), ("port".= 2)]]))

Success ! Configuration: alarm-state-config ... Added

```
saves a configuration that looks like:

```
alarm-state-config:
   tag: 2
   src:
     almKeySrc:
       unSText: onping.plowtech.net
   host:www.stupidurl.com
   port: 2

```
To be requested by a node in the future

### Replacing a Configuration

To replace existing default configuration files, post the new configuration settings to the same name

```
$> post "http://some.lame.nodemanager.com/configure/add" (toJSON (object ["alarm-state-config" .= object  [ ( "tag" .= 2000), ("src" .= (object ["almKeySrc" .= (object [ "unSText" .=  "onping.plowtech.net"])])),  ("host" .= "www.stupidurl.com"), ("port".= 2000)]]))

```
configuration file is now

```
alarm-state-config:
   tag: 2000
   src:
     almKeySrc:
       unSText: onping.plowtech.net
   host:www.stupidurl.com
   port: 2000

```


### Requesting a Configuration
returns a JSON that has the configuration files with your rewrite rules and default configurations.

```
alarm-state-config:
   tag: 2
   src:
     almKeySrc:
       unSText: onping.plowtech.net
   host:www.stupidurl.com
   port: I am a port

```
The client(The Node) requests the config (from the node manager) with:

```

$> post "http://some.lame.nodemanager.com/configure/edit" (toJSON $ object ["configName" .= "alarm-state-config" , "rewrite-rules" .= (object [("key" .= "port") , ("val".= 2)])])

```

returns ...

```
{"alarm-state-config": { "tag": 2, "src":{"almKeySrc":{ "unSText": "onping.plowtech.net"}},  "host":"www.stupidurl.com", "port": 2}}
```

Which the node can then use to configure itself.

### Deleting a Configuration

To delete existing configuration files, post the name of the file.

```
$> post "http://some.lame.nodemanager.com/configure/delete" (toJSON "alarm-state-config")

```

###Copying a Configuration

Another node manager requests the copies of all the configs from another node manager:

```
$> post "http://some.lame.nodemanager.com/configure/copy" (toJSON object[("route".="another.lame.nodemanager.com/configure/add")]) 

```


## Using to monitor Nodes

Important types

``` haskell

data CheckType = GET | POST

data KillMethod = KillUrl Text | KillPID Int | KillNONE

data NodeProcess = NodeProcess {
         , checkName :: Text
         , checkUrl :: Text
         , checkBody :: Value
         , checkMethod :: CheckType
         , checkTime :: Int
         , checkKillMethod :: KillNONE
         
     }
```

### Register a process
(using wreq in this example)

``` haskell

#
$> post "http://node.manager.local/start"  (toJSON $ object ["checkName" .= "AlarmNode1" , "checkUrl" .= "http://10.121.38.159:2233/alarm-status" , "checkBody" .= (toJSON $ object ["alarmId1" .= 3]) , "checkMethod" .= POST 
                                                            , "checkTime" .= 3600 , "checkKillMethod" .= (KillUrl "http://10.121.38.159:2233/kill-me" ) ])
Checking ... OK

New Process Monitor AlarmNode1 started on Sun Jul 20 10:15:33 CDT 2014

```


### Check what is running!

``` haskell

$> get "http://node.manager.local/node?name="AlarmNode1"" 
NodeRunning up since Sun Jul 20 10:15:33 CDT 2014

```


### Tell other node-managers about this one

Node managers can't take over for others, but you can register a url and msg that should occur on these other nodes when something goes wrong.

``` haskell

$> post "http://node.manager.local/register"  (toJSON $ object ["nodeUrls" .= ["url" .= "http://10.121.38.159" , "port" .= 2000
                                                                              ,"url" .= "http://10.121.38.259" , "port" .= 2000
                                                                              ,"url" .= "http://10.121.38.359" , "port" .= 2000
                                                                              ,"url" .= "http://10.121.38.459" , "port" .= 2000 ]
                                                               , "timer" .= 3600
                                                               , onFail .= [ "url" .= "http://10.121.38.159:3111/cryout" , "message" .= (object [msg .= "AlarmNode1 has failed"])
                                                                            ,"method" .= POST ] ])

nodeRegisteredWith http://10.121.38.259 port 2000
nodeRegisteredWith http://10.121.38.359 port 2000
nodeRegisteredWith http://10.121.38.459 port 2000


```



### Configure a Node-Manager

```
node-manager.yml
```

```
url: localhost.com
port: 2000
```















