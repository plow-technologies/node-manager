# node-manager

Node manager is a webserver with background processes designed help monitor node health.
Simple commands let the user control lots of different types of processes. 

node-managers talk to each other to provide redundant monitoring of processes.


## Usage

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

