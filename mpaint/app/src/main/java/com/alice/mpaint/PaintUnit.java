package com.alice.mpaint;

import android.graphics.Paint;
import android.util.DisplayMetrics;

import java.util.Random;

/**
 * Created by alice on 2015/10/4.
 */
public class PaintUnit {
    //static
    int WIDTH_STANDARD = 16;
    //Parameter

    //Handler
    Random random;

    public PaintUnit(){
        random = new Random();
    }

    public byte[] GetColor(byte data){
        byte [] color = new byte[4];
        color[0] = (byte) (127 + data/255*127);
        color[1] = data;
        color[2] = data;
        color[3] = data;
        return color;
    }

    public int GetWidth(byte data){
        int width = WIDTH_STANDARD * (256 - data)/256;
        return width;
    }

    public Paint.Style GetStyle(byte data){
        Paint.Style style;
        if(data<=85){
            style = Paint.Style.FILL;
        } else if (data<=170){
            style = Paint.Style.FILL_AND_STROKE;
        } else {
            style = Paint.Style.STROKE;
        }
        return style;
    }

    public float [] GetPosition(byte data, int screenWidth, int screenHeight){
        float [] position = new float[4];
        position[0] = random.nextInt(screenWidth);/*screenWidth * (data + random.nextInt(5))/256;*/
        position[1] = random.nextInt(screenHeight);/*screenHeight * (data + random.nextInt(5))/256;*/
        position[2] = random.nextInt(screenWidth);/*screenWidth * (data + random.nextInt(25))/256;*/
        position[3] = random.nextInt(screenHeight);/*screenHeight * (data + random.nextInt(25))/256;*/
        return position;
    }

    public float [] GetAngle(){
        float [] angles = new float[2];
        angles[0] = random.nextInt(360);
        angles[1] = random.nextInt(360);
        return angles;
    }

    public int CanvasFact(byte data){
        return (data/32);
    }

    public int GetRadius(){
        return random.nextInt(10);
    }
}
