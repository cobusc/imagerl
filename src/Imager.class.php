<?php
/**
 * @class Imager 
 * @brief An image rendering class with built-in caching support.
 *
 **/
class Imager
{
    const SUPPORTED_FORMATS_KEY="Supported Image Formats";
    const CONFIG_DATA_KEY="Config Data";
    const CONFIG_FILE="../config/config.json";

    private $url = null;
    private $width = 0;
    private $height = 0;
    private $noCache = false;
    private $format = null;
    private $debug = false;
    private $annotation = null;
    private $userAgent = null;
    public  $config = null;

    function __construct($url) //Requires only url. The rest are optional.
    {
        $this->url = $url;
        $this->config = $this->getConfig(self::CONFIG_FILE);
    }

    /*
       The getter and setter functions.
     */

    /**
     * @brief Set the required width of the rendered image.
     *
     * A WURFL lookup will only be done if the width and/or height is set to "wurfl".
     * @param numberOrWurfl - A positive integer or "wurfl".
     **/
    public function setWidth($numberOrWurfl)
    {
         if (!self::isN0orWurfl($numberOrWurfl))
            throw new InvalidArgumentException("Width must be a positive integer or 'wurfl'.");

         if (is_numeric($numberOrWurfl) && (int)$numberOrWurfl > (int)$this->config->maxWidth)
            throw new InvalidArgumentException("Width must be smaller than ".$this->config->maxWidth.".");
  
         $this->width = $numberOrWurfl;
    }

    public function getWidth()
    {
        return $this->width;
    } 

    /**
     * @brief Set the required height of the rendered image.
     *
     * A WURFL lookup will only be done if the width and/or height is set to "wurfl".
     * @param numberOrWurfl - A positive integer or "wurfl".
     **/
    public function setHeight($numberOrWurfl)
    {
         if (!self::isN0orWurfl($numberOrWurfl))
             throw new InvalidArgumentException("Height must be a positive integer or 'wurfl'.");

         if (is_numeric($numberOrWurfl) && (int)$numberOrWurfl > (int)$this->config->maxHeight)
            throw new InvalidArgumentException("Height must be smaller than ".$this->config->maxHeight.".");

         $this->height = $numberOrWurfl;
    }

    public function getHeight()
    {
        return $this->height;
    } 

    /**
     * @brief Prohibit the retrieval of an image from the cache, i.e. force the program do the rendering.
     *
     * @param on True to disable cache
     **/
    public function setNoCache($on)
    {
        if (true === $on || false === $on)
            $this->noCache = $on;
        else
            throw new InvalidArgumentException("Boolean argument expected for function ".__FUNCTION__);
    } 
     
    public function getNoCache()
    {
        return $this->noCache;
    }
 
    /**
     * @brief Enable detailed logging of the steps executed to process the request.
     *
     * @param on True to enable debugging, False to disable.
     **/
    public function setDebug($on)   
    {
        if (true === $on || false === $on)
            $this->debug = $on ? uniqid() : false;
        else
            throw new InvalidArgumentException("Boolean argument expected for function ".__FUNCTION__);
    }

    public function getDebug()
    {
        return $this->debug;
    }

    /**
     * @brief Set the format in which the image should be returned
     *
     * @param format The format in which the image must be returned.
     **/
    public function setFormat($format)
    {
        if (is_string($format))
        {
            $format = strtoupper($format); 
            if (self::isSupportedFormat($format))
                $this->format = $format;
            else
            {
                $msg = "Format '{$this->format}' is not in the list of supported formats.\nTry one of the following:\n".json_encode(Imager::getSupportedFormats());
                throw new InvalidArgumentException($msg);
            }
        }
        else
            throw new InvalidArgumentException("String argument expected for function ".__FUNCTION__);
    }

    public function getFormat()
    {
        return $this->format;
    }

    /**
     * @brief Set the text which must be rendered on the resulting image.
     *
     * @param text The text which will be rendered on the resulting image.
     **/
    public function setAnnotation($text)
    {
        if (is_string($text))
            $this->annotation = empty($text) ? null : $text;
        else
            throw new InvalidArgumentException("String argument expected for function ".__FUNCTION__);
    }

    public function getAnnotation()
    {
        return $this->annotation;
    }

    /**
     * @brief Set the user agent used when performing a WURFL lookup.
     *
     * @param text The user agent string that must be used when performing a WURFL lookup.
     **/
    public function setUserAgent($text)
    {
        if (is_string($text))
            $this->userAgent = empty($text) ? null : $text;
        else
            throw new InvalidArgumentException("String argument expected for function ".__FUNCTION__);
    }

    public function getUserAgent()
    {
        return $this->userAgent;
    }

    /**
     * @brief Creates the key used for identifying the cache entry.
     *
     * A unique identifier must be constructed from all parameters which affects the resulting image.
     * Since most of the parameters are optional, they need to be prefixed by an identifier when
     * constructing the key. This avoids empty parameters causing ambiguous (non-unique) identifiers.
     *
     * The SHA256 hash is used to construct fixed length, uniform keys from the unique identifier constructed.
     *
     * @param url The URL of the image to be retrieved.
     * @param width The requested image width. Defaults to zero.
     * @param height The requested image height. Defaults to zero.
     * @param format The requested image format, e.g. "png". Defaults to an empty string.
     * @param annotation The text rendered on the image, e.g. "Sample". Defaults to an empty string.
     * @return string
     **/
    public static function createKey($url,$width=0,$height=0,$format="",$annotation="")
    {
        $uniqueIdentifier = "{$url}w{$width}h{$height}f{$format}a{$annotation}";

        return hash("sha256", $uniqueIdentifier);
    }

    /**
     * @brief Test whether the specified number is in the range [0, 1, 2, ...] or "wurfl"
     *
     * This function tests whether the arguments is a natural (N) number, or 0
     * (which some people call counting numbers IIRC) or the string "wurfl".
     * @param numberOrWurfl The input to test
     * @return true if numberOrWurfl is an integer greater or equal to zero or "wurfl", else false.
     *
     * Note: Yeah, 4 different ways to describe the set of numbers is excessive. :)
     **/
    static function isN0orWurfl($numberOrWurfl)
    {
        $result = false;
        if (is_numeric($numberOrWurfl))
            $result = $numberOrWurfl >= 0;
        else
            $result = ($numberOrWurfl === "wurfl"); // Must use strict (===) comparison!

        return $result;
    }

    static function getSupportedFormats()
    {
        $supportedFormats = apc_fetch(self::SUPPORTED_FORMATS_KEY);
        if (false === $supportedFormats)
        {
            // There should be some way to get the query formats
            // without having to instantiate an object, surely?
            $dummy = new Imagick();
            $supportedFormats = array_map("strtoupper", $dummy->queryFormats());
            apc_store(self::SUPPORTED_FORMATS_KEY, $supportedFormats);
                
            $dummy->destroy();
        }

        return $supportedFormats;
    }

    /**
     * @brief Check if the specified image format is known to the Imagick library.
     *
     * Returns an error to the user if the specified format is not supported.
     *
     * @param format The format to find.
     * @return void
     **/
    static function isSupportedFormat($format)
    {
        $supportedFormats = self::getSupportedFormats();
        
        return in_array(strtoupper($format), $supportedFormats);
    }

    /**
     * @brief Read the config from the specified config file.
     *
     * The config is cached to avoid repeated reading and parsing of the config file.
     * @param filename The name of the config file to use.
     * @return A JSON object containing the configuration.
     **/
    private function getConfig($filename)
    {
        $config = $this->noCache ? false : apc_fetch(self::CONFIG_DATA_KEY);
        if (false === $config)
        {
            $content = file_get_contents($filename);
            if (false == $content)
                throw new ErrorException("Could not load data from file '{$filename}'.");

            $config = json_decode($content);
            if (NULL === $config)
                throw new ErrorException("Invalid JSON in configuration file '{$filename}'");

            if (false === apc_store(self::CONFIG_DATA_KEY, $config))
                $this->debug_log("Failed to store config to cache.");
        }

        return $config;
    }

    /**
     * @brief Add an annotation (text) to an image.
     *
     * Multiline annotations are not supported at this stage.
     * @param image The image to annotate.
     * @param text The text to add to the image.
     *
     * @return void
     **/
    private static function annotateImage(&$image, $text)
    {
        $draw = new ImagickDraw();
        /* Black text */
        $draw->setFillColor('black');
        /* Font properties */
        $draw->setFont('Courier-Bold'); // Use a fixed-width font

        $width = $image->getImageWidth();
        $height = $image->getImageHeight();

        $radians = atan2($height, $width);
        $degrees = -rad2deg($radians); // Negated to map from trig coordinate system to image.

        $fontHeight = min($width,$height)*0.75; // Initial size estimate.
        $draw->setFontSize($fontHeight); // Sets the font height
        $textProperties = $image->queryFontMetrics($draw, $text);
        $textWidth = $textProperties['textWidth'];
       
        $hypot = hypot($textWidth, $fontHeight);
        $r = atan2($fontHeight, $textWidth); // Not recomputed since angle should stay the same.
        
        $widthLimit = $width / cos($radians-$r);
        $heightLimit = $height / sin($radians+$r);

        $count = 0;
        while ($hypot >= $widthLimit ||
               $hypot >= $heightLimit)
        {
            $fontHeight-=5; // Step size
            $draw->setFontSize($fontHeight); // Sets the font height
            $textProperties = $image->queryFontMetrics($draw, $text);
            $textWidth = $textProperties['textWidth'];
            $hypot = hypot($textWidth, $fontHeight);
            $count++;
        }  

        $draw->setGravity(Imagick::GRAVITY_CENTER);
        /* Create text */
        $image->annotateImage($draw, 0, 0, $degrees, $text);

        $draw->destroy();
    }

    /**
     * @brief Debugging logger helper.
     *
     * Logs a specified string to the default error log if debugging is enabled.
     * Each request has a unique ID assigned to it. This unique ID is prepended to
     * the log lines so that all log lines for a particular request are easily
     * identifiable.
     *
     * @param string The message to log.
     * @return void
     **/
    private function debug_log($string)
    {
        $this->debug && error_log("[{$this->debug}] {$string}");
    }

    /**
     * @brief A helper function to fetch contents from a URL
     *
     * @param url The URL to fetch.
     * @param connectTimeout The allowed to to wait for a connection to be established.
     * @param timeout The allowed time for the URL call to be completed.
     * @return The body of the response as a string.
     **/
    private function curl_get_contents($url, $connectTimeout=0, $timeout=0)
    {
        $con = curl_init();
        curl_setopt($con, CURLOPT_URL, $url);
        curl_setopt($con, CURLOPT_RETURNTRANSFER, 1);
        curl_setopt($con, CURLOPT_FOLLOWLOCATION, 1);
        curl_setopt($con, CURLOPT_FAILONERROR, 1);
        curl_setopt($con, CURLOPT_CONNECTTIMEOUT, $connectTimeout); 
        curl_setopt($con, CURLOPT_TIMEOUT, $timeout); 

        $response = curl_exec($con);
        $info = curl_getinfo($con);

        if (false === $response)
        {
            $this->debug_log("URL ({$url}) returned HTTP code {$info['http_code']}.");
            if (curl_error($con))
                $this->debug_log("Curl error: ".curl_error($con)); 
        }

        return $response;
    }

    /**
     * @brief Perform a WURFL lookup against the configured WURFL webservice.
     *
     * Results are cached locally to prevent unnecessary network calls.
     * @return The WURFL information associated with the useragent.
     **/
    private function wurflLookup()
    {
        $cacheKey = "UA: ".$this->userAgent; // Prefix useragent to prevent malicious cache manipulation.

        $wurflInfo = $this->noCache ? false : apc_fetch($cacheKey);
        if (false === $wurflInfo)
        {
            $wurflUrl = $this->config->wurflWebServiceUrl."?ua=".urlencode($this->userAgent)."&search=max_image_width|max_image_height";
            $this->debug_log("Fetch WURFL information from '$wurflUrl'.");
            $response = $this->curl_get_contents($wurflUrl, $this->config->curlConnectTimeout,
                                                 $this->config->curlTimeout);
            if (false == $response)
                throw new ErrorException("Could not fetch WURFL XML from '$wurflUrl'.");
            else
                $this->debug_log($response);
          
            // simplexml_load_string() can throw an exception! This was not clear in the docs.
            // I wrap the call in a try-catch block so that I can at least print the cause of the error.
            try
            { 
                $wurflData = simplexml_load_string($response);

                if (false == $wurflData)
                    throw new ErrorException("Could not convert WURFL XML from '{$wurflUrl}': {$response}");
            }
            catch (Exception $e)
            {
                throw new ErrorException("Could not convert WURFL XML from '{$wurflUrl}': {$response}");
            }

            $wurflInfo = array();
            //Newer versions of Tera-Wurfl uses "capabilities", not "capability".
            //$wurflInfo["width"] = $wurflData->capabilities->resolution_width;
            //$wurflInfo["height"] = $wurflData->capabilities->resolution_height;

            /*
            $wurflInfo["width"] = (int) $wurflData->device->search->capability[0]->attributes()->value[0];
            $wurflInfo["height"] = (int) $wurflData->device->search->capability[1]->attributes()->value[0];
            */

            $wurflInfo["width"] = (int) $wurflData->device->search->capability[0]["value"];
            $wurflInfo["height"] = (int) $wurflData->device->search->capability[1]["value"];

            if (false === apc_store($cacheKey, $wurflInfo, $this->config->ttl))
                $this->debug_log("Failed to store WURFL lookup to cache.");
        }

        return $wurflInfo;
    }
 
    private function getSourceImageInfo($srcKey)
    {
        $srcImageInfo = $this->noCache ? false : apc_fetch($srcKey);

        if (false === $srcImageInfo)
        {
            $this->debug_log("Source cache entry NOT found. Fetching from '{$this->url}'.");
            try
            {
                $blob = $this->curl_get_contents($this->url, $this->config->curlConnectTimeout,
                                                 $this->config->curlTimeout);
                if (false == $blob)
                    throw new ErrorException("Could not fetch image from '{$this->url}'.");

                $srcImage = new Imagick();
                $srcImage->readImageBlob($blob); // This throws an error if the content returned is not an image.
 
                $this->debug_log("Stripping metadata from source image.");
                if (true !== $srcImage->stripImage())
                    $this->debug_log("Could not strip source image metadata!");

                /** @note: According to some posts the getImageBlob() function sometimes fails
                  * quietly on large images and then simply returns an empty string.
                 **/
                $srcImageInfo["blob"] = $srcImage->getImageBlob();
                $this->debug_log("Source image blob is ".strlen($srcImageInfo["blob"])." bytes long.");

                $srcImageInfo["format"] = $srcImage->getImageFormat();

                $this->debug_log("Storing source image to cache using key '{$srcKey}'.");
                if (false === apc_store($srcKey, $srcImageInfo, $this->config->ttl))
                    $this->debug_log("Error storing source image to cache.");

                $srcImage->destroy();
            }
            catch (ImagickException $e)
            {
                $srcImageInfo["error"] = $e->getMessage();
                if (false === apc_store($srcKey, $srcImageInfo, 60)) // Errors are cached for a minute.
                    $this->debug_log("Error storing image error to cache.");
                throw $e;
            }
        }
        else
            $this->debug_log("Found source image '{$srcKey}' in cache!");

        return $srcImageInfo;
    }

    /** 
     * @brief Renders the image.
     **/
    public function renderImage()
    {
        if ($this->noCache)
            $this->debug_log("Cache lookup prohibited by request.");

        // Since we may have to fiddle with the width/height, we use local copies.
        $width = $this->width;
        $height = $this->height;

        // We have to map "wurfl" dimensions to real values first, since the
        // image cache key is constructed from actual dimensions.
        if ($width === "wurfl" || $height === "wurfl")
        {
            $wurflInfo = $this->wurflLookup();
            // The wurfl dimensions returned may not be higher than the configured maximums.
            if ($width === "wurfl")
                $width = min($wurflInfo["width"], $this->config->maxWidth);

            if ($height === "wurfl")
                $height = min($wurflInfo["height"], $this->config->maxHeight);
        }

        // Check if we have the requested image information in the cache
        $key = self::createKey($this->url, $width, $height, $this->format, $this->annotation);
        $imageInfo = $this->noCache ? false : apc_fetch($key);

        if (false === $imageInfo) // Requested image not in the cache.
        {
            $this->debug_log("Cache entry NOT found. Creating from scratch.");
             
            // Check if we have the source image information in the cache.
            $srcKey = self::createKey($this->url);
            $srcImageInfo = $this->getSourceImageInfo($srcKey);

            if (isset($srcImageInfo["error"]))
                throw new ErrorException("Key $srcKey: {$srcImageInfo["error"]}");

            // At this point we have the source image information. First check if the requested image is not
            // possibly an exact copy of the original, in which case we can simply return what we have.
            // Since the cache keys are uniquely generated based on the request parameters, we can use it for
            // the comparison.
            if ($srcKey == $key)
            {
                 $this->debug_log("Requested image matches source image.");
                 $this->serveImage($srcImageInfo); 
                 return;
            }

            // Construct the requested image from the source image.

            $image = new Imagick();     
            try
            {
                $image->readImageBlob($srcImageInfo["blob"]);
            }
            catch (ImagickException $e)
            {
                $this->debug_log("Bad source image blob.");
                throw $e;
            }

            if ($width>0 || $height>0)
            {
                $this->debug_log("Resizing image from {$image->getImageWidth()}x{$image->getImageHeight()} to {$width}x{$height}.");
                // The function and filter used to create the
                // thumbnail is extremely important when considering
                // performance optimisation. For now I use the standard
                // function.
                $image->thumbnailImage($width, $height, false);
            }

            if ($this->annotation)
            {
                $this->debug_log("Annotating image with '{$this->annotation}'.");
                self::annotateImage($image, $this->annotation);
            }

            $this->debug_log("Image format is '{$image->getImageFormat()}'.");
            if (isset($this->format))
            {
                $this->debug_log("Setting image format to '{$this->format}'.");
                if (true !== $image->setImageFormat($this->format))
                    throw new ErrorException("Could not set the image format to '{$this->format}'.");
            }

            // We do not have to strip the image metadata here anymore. It is already removed
            // form the source image.
            /*
            $this->debug_log("Stripping metadata.");
            if (true !== $image->stripImage())
                $this->debug_log("Could not strip image metadata!");
            */

            /** @note: According to some posts the getImageBlob() function sometimes fails
             * quietly on large images and then simpy returns an empty string.
             **/
            $imageInfo["blob"] = $image->getImageBlob();
            $this->debug_log("Image blob is ".strlen($imageInfo["blob"])." bytes long.");

            $imageInfo["format"] = $image->getImageFormat();

            $this->debug_log("Storing to cache using key '{$key}'.");
            if (false === apc_store($key, $imageInfo, $this->config->ttl))
                $this->debug_log("Error storing image to cache.");

            $image->destroy();
        }
        else // Found requested image in the cache.
            $this->debug_log("Found '{$key}' in cache!");

        // Check if the cached copy contains an error.
        if (isset($imageInfo["error"]))
            throw new Exception("Key $key: {$imageInfo['error']}");

        // All good. Serve it.
        $this->serveImage($imageInfo);
    }

    /**
     * @brief Serves an image. 
     *
     * @param imageInfo All the information related to the image.
     **/
    private function serveImage($imageInfo)
    {
        $this->debug_log("Serving image type '".$imageInfo["format"]."' of size  ".strlen($imageInfo["blob"]));
        header("Content-type: image/".strtolower($imageInfo["format"]));
        // Explicitly disallow caching when WURFL is used.
        if ($this->width === "wurfl" || $this->height === "wurfl")
        {
            header("Cache-Control: no-cache, must-revalidate");
            header("Pragma: no-cache");
            header("Expires: -1");
        }
        echo $imageInfo["blob"];
        $this->debug_log("Done.");
    }
}

?>
