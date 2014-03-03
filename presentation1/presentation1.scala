import scalapipe.dsl._
import scalapipe.kernels._

//currently the low_threshold input comes from mean_var!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
object Everything extends App{
	//zero encoding kernel
	val WriteFile = new Kernel ("WriteFile")
	{
		val in = input(UNSIGNED32)
		val fname = config(STRING,'filename)
		val temp = local(UNSIGNED32)

		val fd = local(stdio.FILEPTR,0)

		if(fd == 0) 
		{
			fd = stdio.fopen(fname,"wb")
			if (fd == 0) 
			{
				stdio.printf("ERROR: could not open %s\n", fname)
				stdio.exit(-1)
			}
		} 

		temp = in
		stdio.fprintf(fd, "%x \n", temp)
	}
	
  val CountConsecutive = new Kernel ("CountConsecutive")
	{
		//initialization
		val pixel_processed = local(UNSIGNED32,0)
		val frame_size = local(UNSIGNED32,100)
		val start = local(UNSIGNED32,0)
		val pre_inte = local(UNSIGNED32,0)
		val cur_inte = local(UNSIGNED32,0)
		val count = local (UNSIGNED32,0)
		//var wrt = new write(UNSIGNED32,STRING)
		val pixelIn = input(UNSIGNED32)
		val result = output(UNSIGNED32)

		if(start == 0) //reading in first pixel
		{
			pre_inte = pixelIn
			pixel_processed = 1
			count = 1
			start = 1
		}
		//read in next pixel and compare to previous pixel
		cur_inte = pixelIn
		pixel_processed = pixel_processed + 1
    
    if(cur_inte == pre_inte)
    {
      count = count + 1
    }
    else // if (cur != prev)
    {
      // send out the count so far and the prev char
      result = (pre_inte << 16) + count
      
      // start a new counter
      pre_inte = cur_inte
      count = 1
    }
    // If the last pixel - send the output
    if(pixel_processed == frame_size)
    {
      result = (pre_inte << 16) + count
      start = 0
    }
  }
  
	
	//mean-var kernel
	//Mean computing
	val mean_var = new Kernel ("mean_var")
	{
		val height = 40
		val width = 40
		val frame = height*width
		
		val temp = local(UNSIGNED32,0)

		//initialize
		val count = local(UNSIGNED32,1)
		val round_counter = local(UNSIGNED32,0)

		val sum = local(Vector(UNSIGNED16, frame))
		val square_sum = local(Vector(UNSIGNED32, frame))
		
		val pixelIn   = input(UNSIGNED32)
		val pixelOut  = output(UNSIGNED32)
		val meanVal1    = output(UNSIGNED32)
		val meanVal2  = output(UNSIGNED32)

		//update
		temp = pixelIn

		sum(round_counter) += temp
		square_sum(round_counter) += temp*temp

		val mean = sum(round_counter)/count
		val vari = square_sum(round_counter)/count - mean*mean

		pixelOut = temp
		meanVal1 = (mean << 16) | vari
		meanVal2 = (mean << 16) | vari
		round_counter += 1

		if(round_counter >= frame)
		{
			round_counter = 0
			count += 1
		}

	}
	
	//thresholding kernel
	val pixelDataOpt = new Kernel ("pixelDataOpt")
	{
		//optimize reading on the fly output
		val pixel_in = input(UNSIGNED32)
		val low_threshold_in = input(UNSIGNED32)
		val resultf = output(UNSIGNED32)
		val result = local(UNSIGNED32,0)
		
		val ArrayType = Vector(UNSIGNED32,42)
		val pixel_array_a = local(ArrayType)
		val boolean_array_a = local(ArrayType)
		val pixel_array_b = local(ArrayType)
		val boolean_array_b = local(ArrayType)
		val pixel_array_c = local(ArrayType)
		val boolean_array_c = local(ArrayType)
		val low_threshold = local(UNSIGNED32,1)
		val start = local(UNSIGNED32,1)
		val pixel_count = local(UNSIGNED32,1)
		
		
		for (i <- 0 until 42){
			pixel_array_a(i) = 0
		}
		if (start == 1)
		{
			pixel_array_b(0) = 0
			while (pixel_count < 41){
				pixel_array_b(pixel_count) = pixel_in
				low_threshold = low_threshold_in
				if(pixel_array_b(pixel_count) < low_threshold)
				{
					boolean_array_b(pixel_count) = 0
				}
				else
				{
					boolean_array_b(pixel_count) = 1
				}
				pixel_count = pixel_count + 1
			}
			pixel_count = 1
			pixel_array_b(41) = 0
			start = 2
		}
		else if (start == 2)
		{
			pixel_array_c(0) = 0
			boolean_array_c(0) = 0
			
			while (pixel_count < 41){
				pixel_array_c(pixel_count) = pixel_in
				low_threshold = low_threshold_in
				if(pixel_array_c(pixel_count) < low_threshold)
				{
					boolean_array_c(pixel_count) = 0
				}
				else
				{
					boolean_array_c(pixel_count) = 1
				}
				if(pixel_count >=2)
				{
					val i = pixel_count-1
					result = pixel_array_b(i)
					result = boolean_array_a(i-1) | (result << 1)
					result = boolean_array_a(i) | (result << 1)
					result = boolean_array_a(i+1) | (result << 1)
					result = boolean_array_b(i-1) | (result << 1)
					result = boolean_array_b(i+1) | (result << 1)
					result = boolean_array_c(i-1) | (result << 1)
					result = boolean_array_c(i) | (result << 1)
					result = boolean_array_c(i+1) | (result << 1)
				
					resultf = result 
				}
				pixel_count = pixel_count + 1
			}
			
			pixel_array_c(41) = 0
			
			val i = pixel_count-1
			result = pixel_array_b(i)
			result = boolean_array_a(i-1) | (result << 1)
			result = boolean_array_a(i) | (result << 1)
			result = boolean_array_a(i+1) | (result << 1)
			result = boolean_array_b(i-1) | (result << 1)
			result = boolean_array_b(i+1) | (result << 1)
			result = boolean_array_c(i-1) | (result << 1)
			result = boolean_array_c(i) | (result << 1)
			result = boolean_array_c(i+1) | (result << 1)
				
			resultf = result 
			
			pixel_count = 1
			start = 3
		}
		else if(start == 41)
		{
			pixel_array_a = pixel_array_b
			boolean_array_a = boolean_array_b
			pixel_array_b = pixel_array_c
			boolean_array_b = boolean_array_c
			
			for(i <- 1 until 41)
			{
				result = pixel_array_b(i)
				result = boolean_array_a(i-1) | (result << 1)
				result = boolean_array_a(i) | (result << 1)
				result = boolean_array_a(i+1) | (result << 1)
				result = boolean_array_b(i-1) | (result << 1)
				result = boolean_array_b(i+1) | (result << 1)
				result = result << 3
				
				resultf = result 
			}
			
			start = 1
			
		}
		else
		{
			pixel_array_a = pixel_array_b
			boolean_array_a = boolean_array_b
			pixel_array_b = pixel_array_c
			boolean_array_b = boolean_array_c
			
			pixel_array_c(0) = 0
			while (pixel_count < 41){
				pixel_array_c(pixel_count) = pixel_in
				low_threshold = low_threshold_in
				if(pixel_array_c(pixel_count) < low_threshold)
				{
					boolean_array_c(pixel_count) = 0
				}
				else
				{
					boolean_array_c(pixel_count) = 1
				}
				if(pixel_count >=2)
				{
					val i = pixel_count-1
					result = pixel_array_b(i)
					result = boolean_array_a(i-1) | (result << 1)
					result = boolean_array_a(i) | (result << 1)
					result = boolean_array_a(i+1) | (result << 1)
					result = boolean_array_b(i-1) | (result << 1)
					result = boolean_array_b(i+1) | (result << 1)
					result = boolean_array_c(i-1) | (result << 1)
					result = boolean_array_c(i) | (result << 1)
					result = boolean_array_c(i+1) | (result << 1)
				
					resultf = result 
				}
				pixel_count = pixel_count + 1
			}
			pixel_array_c(41) = 0
			
			val i = pixel_count-1
			result = pixel_array_b(i)
			result = boolean_array_a(i-1) | (result << 1)
			result = boolean_array_a(i) | (result << 1)
			result = boolean_array_a(i+1) | (result << 1)
			result = boolean_array_b(i-1) | (result << 1)
			result = boolean_array_b(i+1) | (result << 1)
			result = boolean_array_c(i-1) | (result << 1)
			result = boolean_array_c(i) | (result << 1)
			result = boolean_array_c(i+1) | (result << 1)
				
			resultf = result 
			
			pixel_count = 1
			
			start = start + 1
		}
		
	}
	
	val threshold = new Kernel ("threshold") { 

		val pixel_word = input(UNSIGNED32)
		val low_threshold_in = input(UNSIGNED32)
		val pixel_out = output(UNSIGNED32)
		val low_thresh = local(UNSIGNED32,2)
		val high_thresh = local(UNSIGNED32,128)
		val input = local(UNSIGNED32)
		val neighbor = local(UNSIGNED32)
		val orig_pixel = local(UNSIGNED32)
		val temp = local(UNSIGNED32,0)
		
		input = pixel_word
		low_thresh = low_threshold_in
		
		orig_pixel = input >> 8
		neighbor = input & 0xFF
		
		
		if(orig_pixel < low_thresh){
			temp = 0
		}
		else if(orig_pixel > high_thresh){
			temp = orig_pixel
		}
		else if(orig_pixel > low_thresh && orig_pixel < high_thresh){
			if(neighbor > 0){
				temp = orig_pixel
			}
			else if(neighbor == 0){
				temp = 0
			}
		}
		else{
			temp = 0xFFFF
		}
		pixel_out = temp
	}

  //count kernel
	val BufferFramesAndSplit = new Kernel ("BufferFramesAndSplit")
  {
    val pixelIn = input(UNSIGNED32)
    val output1 = output(UNSIGNED32)
    val output2 = output(UNSIGNED32)
    val deliverOutput1 = local(UNSIGNED32,1)
    val frameSize = local(UNSIGNED32,100)
    val totalNumOfPixels = local(UNSIGNED32,1600) // 16 frames in image. 100 pixels each frame
    val storeCounter = local(UNSIGNED32,0)
    val base = local(UNSIGNED32,0)
    val deliverMode = local(UNSIGNED32,0)
    val ArrayType = Vector(UNSIGNED32,1600)
    val frameArray = local(ArrayType)
    
    
    // store the pixel in a vector
    if(storeCounter <= totalNumOfPixels)
    {
      frameArray(storeCounter) = pixelIn
      storeCounter = storeCounter + 1
    }
    // Read all pixels, switch to deliver mode
    if(storeCounter == totalNumOfPixels)
    {
        deliverMode = 1
        base = 0
    }

    if(deliverMode == 1) // deliver next pixel
    {
      for(f <- 0 until 100) // frames scale
      {
        if(deliverOutput1 == 1)
        {
          for(i <- 0 until frameSize) // frame size
          {
            output1 = frameArray(base + i)
          }
          deliverOutput1 = 0
        }
        else // send to output2
        {
          for(i <- 0 until frameSize) // frame size
          {
            output2 = frameArray(base + i)
          }
          deliverOutput1 = 1
        }
        base += frameSize
        if(base == totalNumOfPixels)
          base = 0
      }
    }
  }
  val BufferFrames = new Kernel ("BufferFrames")
  {
    val pixelIn = input(UNSIGNED32)
    val output1 = output(UNSIGNED32)
    val frameSize = local(UNSIGNED32,100)
    val totalNumOfPixels = local(UNSIGNED32,1600) // 16 frames in image. 100 pixels each frame
    val storeCounter = local(UNSIGNED32,0)
    val base = local(UNSIGNED32,0)
    val deliverMode = local(UNSIGNED32,0)
    val ArrayType = Vector(UNSIGNED32,1600)
    val frameArray = local(ArrayType)
    
    
    // store the pixel in a vector
    if(storeCounter <= totalNumOfPixels)
    {
      frameArray(storeCounter) = pixelIn
      storeCounter = storeCounter + 1
    }
    // Read all pixels, switch to deliver mode
    if(storeCounter == totalNumOfPixels)
    {
        deliverMode = 1
        base = 0
    }

    if(deliverMode == 1) // deliver next pixel
    {
      for(f <- 0 until 100) // frames scale
      {
        for(i <- 0 until frameSize) // frame size
        {
          output1 = frameArray(base + i)
        }
        base += frameSize
        if(base == totalNumOfPixels)
          base = 0
      }
    }
  }


	val dropSize = new Drop(SIGNED32) 
  val dropPixel = new Drop(UNSIGNED32)
  
	val app = new Application
	{
		val in = BMPReader('file -> "../../white.bmp")
		dropSize(in(1))
		dropSize(in(2))
    val pixel = BufferFrames(in(0))
		val meanvar = mean_var(pixel)
		val pix = pixelDataOpt(meanvar(0),meanvar(1))
		val filtered = threshold(pix,meanvar(2))
		val result = CountConsecutive(filtered)
		WriteFile(result,'filename -> "compressed.txt")

	}
	app.emit("presentation1")
	
}