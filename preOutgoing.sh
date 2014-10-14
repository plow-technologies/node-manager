cp ./dist/build/node-manager/node-manager ./
tar -czvf node-manager.tar.gz node-manager
rsync -avzhe ssh --progress node-manager.tar.gz node@alarm.plowtech.net:/home/node/incoming/
rm ./node-manager
rm ./node-manager.tar.gz
