package com.alice.mpaint;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.util.AttributeSet;
import android.util.Log;
import android.view.SurfaceHolder;
import android.view.SurfaceView;

/**
 * Created by alice on 2016/1/2.
 */
public class PianoPaintView extends SurfaceView implements SurfaceHolder.Callback{

    SurfaceHolder holder;
    Canvas canvas = null;

    int width;
    int height;

    public PianoPaintView(Context context) {
        super(context);
        holder = this.getHolder();
        holder.addCallback(this);
    }

    public PianoPaintView(Context context,  AttributeSet attrs) {
        super(context, attrs);
        holder = this.getHolder();
        holder.addCallback(this);

    }
    public Paint.Style GetStyle(int data){
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

    public void drawPicture(Canvas canvas, int tone) {
        //Reset the canvas
        Log.d("alice_debug", "I am entering the draw thread");
        canvas = holder.lockCanvas();
        Paint pan = new Paint();
        pan.setAntiAlias(true);
        pan.setColor(GetColor(tone));
        //pan.setARGB(128, color[1], color[2], color[3]);
        pan.setStyle(GetStyle(tone));
        pan.setStrokeWidth(GetWidth(tone));
        float[] position = GetPosition(width, height);
        float[] angle = GetAngles();

        switch (tone/30) {
            case 0:
                //Draw Rect
                canvas.drawRect(position[0], position[1], position[2], position[3], pan);
                break;
            case 1:
                //Draw Circle
                canvas.drawCircle(position[0], position[1], (float) (Math.random()*10), pan);
                break;
            case 2:
                //Draw RoundRect
                //      canvas.drawRoundRect(position[0], position[1], position[2], position[3],
                //              mpu.getRadius(), mpu.getRadius(), pan);
                //     break;
            case 3:
                //Draw Oval
                //                   canvas.drawOval(position[0], position[1], position[2], position[3], pan);
                //                   break;
            case 4:
                //Draw Vertics
                canvas.drawVertices(Canvas.VertexMode.TRIANGLES, 4, position, 0, position, 0,
                        null, 0, null, 0, 0, pan);
                break;
            case 5:
                //Draw Arc
                //                canvas.drawArc(position[0], position[1], position[2], position[3], angle[0],
                //                        angle[1], false, pan);
                break;
            case 6:
                //Draw Lane
                canvas.drawLine(position[0], position[1], position[2], position[3], pan);
                break;
            case 7:
                //Draw Point
                canvas.drawPoint(position[0], position[1], pan);
                break;
            default:
                break;
        }
        holder.unlockCanvasAndPost(canvas);
    }

    private int GetColor(int tone) {
        int color;
        switch (tone/30) {
            case 0:
                color = Color.BLUE;
                break;
            case 1:
                color = 0xFF9c27b0;
                break;
            case 2:
                color = Color.GREEN;
                break;
            case 3:
                color = Color.YELLOW;
                break;
            case 4:
                color = 0xffe91e63;
                break;
            case 5:
                color = 0xfff44336;  // Orange
                break;
            case 6:
                color = Color.RED;
                break;
            default:
                color = Color.BLUE;
                break;
        }
        return color;
    }

    private float[] GetAngles() {
        float [] angles = new float[2];
        angles[0] = (float) (360*Math.random());
        angles[1] = (float) (360*Math.random());
        return angles;
    }


    public float [] GetPosition(int screenWidth, int screenHeight) {
            float[] position = new float[4];
            position[0] = (float)((float)screenWidth * Math.random());
            position[1] = (float)((float)screenHeight * Math.random());/*screenHeight * (data + random.nextInt(5))/256;*/
            position[2] = (float)((float)screenWidth * Math.random());/*screenWidth * (data + random.nextInt(25))/256;*/
            position[3] = (float)((float)screenHeight * Math.random());/*screenHeight * (data + random.nextInt(25))/256;*/
            return position;
        }

    private float GetWidth(int tone) {
        int width = tone/100;
        return width;
    }

    @Override
    public void surfaceCreated(SurfaceHolder holder) {

    }

    @Override
    public void surfaceChanged(SurfaceHolder holder, int format, int width, int height) {

    }

    @Override
    public void surfaceDestroyed(SurfaceHolder holder) {

    }

    public void setTone(String tone) {
        int value = 0;
        switch (tone) {
            case "Do":
                value = 0;
                break;
            case "Re":
                value = 30;
                break;
            case "Mi":
                value = 60;
                break;
            case "Fa":
                value = 90;
                break;
            case "So":
                value = 120;
                break;
            case "La":
                value = 150;
                break;
            case "Xi":
                value = 180;
                break;
            default:
                break;
        }
        drawPicture(canvas, value);
    }

    public void ConfigView(int widthPixels, int heightPixels) {
        width = widthPixels;
        height = heightPixels;
    }
}

