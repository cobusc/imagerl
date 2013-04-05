<?php

/**
 * @file imager.php
 */

include_once("Imager.class.php");

try
{
    if (!array_key_exists("url", $_REQUEST))
        throw new InvalidArgumentException("Parameter URL not specified.");
    
    $imager = new Imager($_REQUEST["url"]);

    if (array_key_exists("width", $_REQUEST))
        $imager->setWidth($_REQUEST["width"]);

    if (array_key_exists("height", $_REQUEST))
        $imager->setHeight($_REQUEST["height"]);

    if (array_key_exists("nocache", $_REQUEST))
        $imager->setNoCache(true);

    if (array_key_exists("format", $_REQUEST))
        $imager->setFormat($_REQUEST["format"]);

    if (array_key_exists("debug", $_REQUEST))
        $imager->setDebug(true);

    if (array_key_exists("annotation", $_REQUEST))
        $imager->setAnnotation($_REQUEST["annotation"]);

    if (array_key_exists("ua", $_REQUEST))
        $imager->setUserAgent($_REQUEST["ua"]);
    else if (array_key_exists("HTTP_USER_AGENT", $_SERVER))
        $imager->setUserAgent($_SERVER["HTTP_USER_AGENT"]);

    $imager->renderImage();
}
catch (InvalidArgumentException $e)
{
    header("HTTP/1.1 400 Bad Request", true, 400);
    echo $e->getMessage();
    error_log($e->getMessage());
}
catch (Exception $e)
{
    header("HTTP/1.1 401 Other Error", true, 401);
    echo $e->getMessage();
    error_log($e->getMessage());
//    trigger_error($e->getMessage());
}

?>
