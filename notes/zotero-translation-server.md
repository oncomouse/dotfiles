docker pull zotero/translation-server
docker run -d -p 1969:1969 --restart always --name translation-server zotero/translation-server
