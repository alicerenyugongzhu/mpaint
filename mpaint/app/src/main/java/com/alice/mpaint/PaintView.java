package com.alice.mpaint;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.util.AttributeSet;
import android.util.Log;
import android.view.SurfaceHolder;
import android.view.SurfaceView;

/**
 * Created by alice on 2015/12/5.
 */
//public class PaintView extends View implements Runnable{
public class PaintView extends SurfaceView implements SurfaceHolder.Callback{

    MusicPaintUtils mpu = null;
    SurfaceHolder holder;
    private MyThread myThread;
    Canvas wholeCanvas = null;

    public PaintView (Context context){
        super(context);
    }

    public PaintView(Context context, AttributeSet attrs){
        super(context, attrs);

    }


    public PaintView(Context context, MusicPaintUtils mpu) {
        super(context);
        this.mpu = mpu;
        //mpu.PaintUpdate();
        InitHolder();

    }

    private void InitHolder(){
        holder = this.getHolder();
        holder.addCallback(this);
        myThread = new MyThread(holder);
    }

    class MyThread extends Thread {
        private SurfaceHolder holder;
        public boolean isRun ;

        public  MyThread(SurfaceHolder holder)
        {
            this.holder =holder;
            isRun = true;
        }
        public void drawPicture(Canvas canvas) {
            //Reset the canvas
            Log.d("alice_debug", "I am entering the draw thread");
            canvas = holder.lockCanvas();
            Paint pan = new Paint();
            pan.setAntiAlias(true);
            byte[] color = mpu.getColor();
            pan.setARGB(color[0], color[1], color[2], color[3]);
            //pan.setARGB(128, color[1], color[2], color[3]);
            pan.setStyle(mpu.getStyle());
            pan.setStrokeWidth(mpu.getWidth());
            float[] position = mpu.getPosition();
            float[] angle = mpu.getAngles();

            switch (mpu.getCanvasFact()) {
                case 0:
                    //Draw Rect
                    canvas.drawRect(position[0], position[1], position[2], position[3], pan);
                    break;
                case 1:
                    //Draw Circle
                    canvas.drawCircle(position[0], position[1], mpu.getRadius(), pan);
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

        @Override
        public void run() {

            while(isRun) {
                while (!Thread.currentThread().isInterrupted()) {
                    try {
                        Thread.sleep(500);
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                    }
                    mpu.PaintUpdate();
                    drawPicture(wholeCanvas);
                    //postInvalidate();
                }
            }
        }
    }

    @Override
    public void surfaceCreated(SurfaceHolder holder) {
        myThread.isRun = true;
        myThread.start();
    }

    @Override
    public void surfaceChanged(SurfaceHolder holder, int format, int width, int height) {

    }

    @Override
    public void surfaceDestroyed(SurfaceHolder holder) {
        myThread.isRun = false;
    }
}

