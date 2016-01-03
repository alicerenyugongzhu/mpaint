package com.alice.mpaint;

import android.content.Context;
import android.database.Cursor;
import android.graphics.Paint;
import android.graphics.Path;
import android.net.Uri;
import android.util.Log;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;

/**
 * Created by alice on 2015/10/4.
 */
public class MusicPaintUtils {
    //Constant
    final byte [] FRAME_TAG = {-1, -5};

    //Parameter
    int screenWidth;
    int screenHeight;
    boolean endPaint = false;
    byte[] color;
    float [] position;
    Paint.Style style;
    int width;
    int radius;
    float [] angles;
    int canvasFact;
    private String path;
    int frameStart;
    byte frameDataPosition;
    byte [] all;
    int fileLength;

    //Handler
    PaintUnit PaintUnit = null;
    Mp3Frame frame;


    public MusicPaintUtils(){
        PaintUnit = new PaintUnit();
        color = new byte[4];
        position = new float[4];
        angles = new float[2];
        frameDataPosition = 0;
    }

    public int getScreenWidth() {
        return screenWidth;
    }

    public void setScreenWidth(int screenWidth) {
        this.screenWidth = screenWidth;
    }

    public int getScreenHeight() {
        return screenHeight;
    }

    public void setScreenHeight(int screenHeight) {
        this.screenHeight = screenHeight;
    }

    public boolean FindFrameStart(String fileName){
        try {
            FileInputStream inStream = new FileInputStream(fileName);
            //byteArrayOutputStream outStream = new byteArrayOutputStream();
            fileLength = inStream.available();
            all = new byte [fileLength];
            inStream.read(all);
            //byte buffer;
            int length = fileLength;
            int len = 0;
            boolean found = false;
            while(length > 0){
                if(all[len] == FRAME_TAG[0] && all[len+1] == FRAME_TAG[1]) {
                    found = true;
                    break;
                } else {
                    len = len + 1;
                    length = length - 1;
                }
            }
            //inStream.close();
            if(found) {
                frameStart = len;
                Log.d("alice_debug", "found the frame start " + frameStart);
                SetFrame(frameStart, fileName);
                Log.d("alice_debug", "I am going to return true");
                return true;
            }
            Log.d("alice_debug", "I am going to return false");
            return false;
        } catch (Exception e) {
            Log.d("alice_debug", "I am in the exception");
            e.printStackTrace();
            return false;
        }
    }

    private void SetFrame(int frameStart, String fileName) {
            Log.d("alice_debug", "I am in the SetFrame. fileLength is " + fileLength);
            if(frameStart >= fileLength){
                endPaint = true;
                return;
            }
            frame = new Mp3Frame(1, 1, true, (all[frameStart+2]>>4)&0xf, (all[frameStart+2]>>2) & 0x3);
            Log.d("alice_debug", "I am begining to set Data. Fram Data Length is " + frame.getDataLength());
            Log.d("alice_debug", "frameStart is " + frameStart);
            for(int i = 0; i < frame.getDataLength(); i++) {
                Log.d("alice_debug", "I am in " + i + " loop");
                frame.setData(all[frameStart + 4 + i], i);
            }
        Log.d("alice_debug", "I am finishing the setFrame");
    }

    public static String GetPath(Context context, Uri uri) {
            // TODO Auto-generated method stub
            if("content".equalsIgnoreCase(uri.getScheme())){
                String[] projection = {"_data"};
                Cursor cursor = null;

                try{
                    cursor = context.getContentResolver().query(uri, projection, null, null, null);
                    int column_index = cursor.getColumnIndexOrThrow("_data");
                    if(cursor.moveToFirst()){
                        return cursor.getString(column_index);
                    }
                } catch (Exception e){

                }
            }
            else if("file".equalsIgnoreCase(uri.getScheme())){
                return uri.getPath();
            }

            return null;
    }

    public void PaintUpdate() {
        if(!endPaint) {
            if(frameDataPosition >= frame.getDataLength()) {
                frameStart = FindNextFrameStart(frameStart);
                SetFrame(frameStart, path);
            }
            byte[] data = frame.getData();
            SetPaintParam(data[frameDataPosition]);
            frameDataPosition++;

        }
    }

    public void PianoPaintUpdate(){

    }

    public void SetPaintParam(byte data){
        color = PaintUnit.GetColor(data);
        position = PaintUnit.GetPosition(data, screenWidth, screenHeight);
        style = PaintUnit.GetStyle(data);
        width = PaintUnit.GetWidth(data);
        radius = PaintUnit.GetRadius();
        angles = PaintUnit.GetAngle();
        canvasFact = PaintUnit.CanvasFact(data);
    }

    private int FindNextFrameStart(int frameStart) {
        int newFrameStart = frameStart + frame.getDataLength() + 4;
            if(newFrameStart >= fileLength){
                endPaint = true;
                return newFrameStart;
            }
            if(all[newFrameStart-1] == FRAME_TAG[0]){
                newFrameStart = newFrameStart-1;
            } else if(all[newFrameStart+1] == FRAME_TAG[0]) {
                newFrameStart = newFrameStart + 1;
            }
        return newFrameStart;
    }

    public byte[] getFRAME_TAG() {
        return FRAME_TAG;
    }

    public byte[] getColor() {
        return color;
    }

    public float[] getPosition() {
        return position;
    }

    public Paint.Style getStyle() {
        return style;
    }

    public int getWidth() {
        return width;
    }

    public int getRadius() {
        return radius;
    }

    public float[] getAngles() {
        return angles;
    }

    public int getCanvasFact() {
        return canvasFact;
    }

    public void setPath(String path) {
        this.path = path;
    }
}
