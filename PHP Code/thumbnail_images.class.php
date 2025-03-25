<?php
/*
  +----------------------------------------------------------------------+
  | Thumbnail Image v.: 0.2                                              |
  +----------------------------------------------------------------------+
  | Generate thumbnail images                                            |
  +----------------------------------------------------------------------+
*/

class ThumbnailImages {

    // Properties
    private $pathImgOld;
    private $pathImgNew;
    private $newWidth;
    private $newHeight;
    private $mime;

    // Setters
    public function setPaths($oldPath, $newPath) {
        $this->pathImgOld = $oldPath;
        $this->pathImgNew = $newPath;
    }

    public function setDimensions($width, $height) {
        $this->newWidth = $width;
        $this->newHeight = $height;
    }

    // Method to save the new image
    private function imagejpegNew($newImg, $pathImg) {
        switch ($this->mime) {
            case 'image/jpeg':
            case 'image/pjpeg':
                return imagejpeg($newImg, $pathImg);
            case 'image/gif':
                return imagegif($newImg, $pathImg);
            case 'image/png':
                return imagepng($newImg, $pathImg);
            default:
                return false;
        }
    }

    // Method to create an image resource from the old image
    private function imagecreateFromNew($pathImg) {
        switch ($this->mime) {
            case 'image/jpeg':
            case 'image/pjpeg':
                return imagecreatefromjpeg($pathImg);
            case 'image/gif':
                return imagecreatefromgif($pathImg);
            case 'image/png':
                return imagecreatefrompng($pathImg);
            default:
                return false;
        }
    }

    // Method to create the thumbnail
    public function createThumbnail() {
        $oldSize = @getimagesize($this->pathImgOld);
        if ($oldSize === false) {
            return false; // Return false if the image cannot be loaded
        }
        
        $this->mime = $oldSize['mime'];
        $oldWidth = $oldSize[0];
        $oldHeight = $oldSize[1];
        
        // Calculate new dimensions if not set
        if ($this->newHeight === null && $this->newWidth !== null) {
            $this->newHeight = ceil(($oldHeight * $this->newWidth) / $oldWidth);
        } elseif ($this->newWidth === null && $this->newHeight !== null) {
            $this->newWidth = ceil(($oldWidth * $this->newHeight) / $oldHeight);
        } elseif ($this->newHeight === null && $this->newWidth === null) {
            return false; // Neither dimension is set
        }
        
        // Calculate cropping positions
        $heightCastr = ceil(($oldWidth * $this->newHeight) / $this->newWidth);
        $bottomCastr = ($oldHeight - $heightCastr) / 2;
        
        $widthCastr = ceil(($oldHeight * $this->newWidth) / $this->newHeight);
        $rightCastr = ($oldWidth - $widthCastr) / 2;
        
        // Adjust cropping positions based on the calculated values
        if ($bottomCastr > 0) {
            $widthCastr = $oldWidth;
            $rightCastr = 0;
        } elseif ($rightCastr > 0) {
            $heightCastr = $oldHeight;
            $bottomCastr = 0;
        } else {
            $widthCastr = $oldWidth;
            $heightCastr = $oldHeight;
            $rightCastr = 0;
            $bottomCastr = 0;
        }
        
        $oldImg = $this->imagecreateFromNew($this->pathImgOld);
        if ($oldImg) {
            $newImgCastr = imagecreatetruecolor($widthCastr, $heightCastr);
            imagecopyresampled($newImgCastr, $oldImg, 0, 0, $rightCastr, $bottomCastr, $widthCastr, $heightCastr, $widthCastr, $heightCastr);
            
            $newImg = imagecreatetruecolor($this->newWidth, $this->newHeight);
            imagecopyresampled($newImg, $newImgCastr, 0, 0, 0, 0, $this->newWidth, $this->newHeight, $widthCastr, $heightCastr);
            
            imagedestroy($newImgCastr);
            imagedestroy($oldImg);
            
            if (!$this->imagejpegNew($newImg, $this->pathImgNew)) {
                return false; // Failed to save the new image
            }
            
            imagedestroy($newImg);
            return true; // Success
        }
        
        return false; // Failed to create the image
    }
}
?>
