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

### Using the client

Servant client can be used for easy haskell interface. Future examples of using the client will assume you have done the following

```
import Node.Client

conf <- readNodeManagerConf <config file path>
api <- makeNodeAPI conf

```
client functions are stored in API and can now be accessed in the following manner

```
addCfg api      -- Takes a value and adds it to Node Manager
retrieveCfg api -- Takes a config name and returns the associated config
editCfg api     -- Takes a value containing changes to a config and applies those changes
deleteCfg api   -- Takes a value and deletes the associated config from Node Manager
copyCfg api     -- Takes a value including a node manager address and copies the configs from that node manager

```

### Adding a Configuration

To add a new configuration to serve to nodes, use the client or post...

```
$> (addCfg api) <Value Object just like post example>

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

### Editing a Configuration

To edit existing default configuration files, use the client or post the settings you wish to change to /configure/edit

```
$> (editCfg api) <Value Object just like post example>

$> post "http://some.lame.nodemanager.com/configure/edit" (toJSON (object ["configName" .= "alarm-state-config", "rewrite-rules" .= [object ["key" .= "port", "val" .= 4]]]))

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

### Replacing a Configuration

To replace existing default configuration files, use the client or post the new configuration settings to the same name

```
$> (addCfg api) <Value Object just like post example>

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
$> (retrieveCfg api) <Value Object just like post example>

$> post "http://some.lame.nodemanager.com/configure/retrieve" (toJSON $ object ["configName" .= "alarm-state-config" , "rewrite-rules" .= (object [("key" .= "port") , ("val".= 2)])])

```

returns ...

```
{"alarm-state-config": { "tag": 2, "src":{"almKeySrc":{ "unSText": "onping.plowtech.net"}},  "host":"www.stupidurl.com", "port": 2}}
```

Which the node can then use to configure itself.

### Deleting a Configuration

To delete existing configuration files, post the name of the file.

```
$> (deleteCfg api) <Value Object just like post example>

$> post "http://some.lame.nodemanager.com/configure/delete" (toJSON "alarm-state-config")

```

###Copying a Configuration

Another node manager requests the copies of all the configs from another node manager:

```
$> (copyCfg api) <Value Object just like post example>

$> post "http://some.lame.nodemanager.com/configure/copy" (toJSON object[("route".="another.lame.nodemanager.com/configure/add")]) 

```
### Configure a Node-Manager

```
node-manager.yml
```

```
url: localhost.com
port: 2000
```















