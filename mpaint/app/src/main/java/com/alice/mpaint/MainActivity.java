package com.alice.mpaint;


import android.annotation.TargetApi;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.database.Cursor;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Path;
import android.media.MediaPlayer;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.SurfaceHolder;
import android.view.SurfaceView;
import android.view.View;
import android.widget.Button;
import android.widget.Toast;

import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.IOException;

public class MainActivity extends Activity {

    //Parameters
    final int REQUEST_1 = 1;

    //Handler
    MediaPlayer mp = null;
    MusicPaintUtils mpu = null;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        DisplayMetrics dm =getResources().getDisplayMetrics();
        getWindowManager().getDefaultDisplay().getMetrics(dm);

        mpu = new MusicPaintUtils();
        mpu.setScreenHeight(dm.heightPixels);
        mpu.setScreenWidth(dm.widthPixels);

        Button filePicker = (Button) findViewById(R.id.paint_mp3);
        View.OnClickListener listener = new View.OnClickListener() {

            @Override
            public void onClick(View arg0) {
                // TODO Auto-generated method stub
                Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
                intent.setType("audio/*");
                intent.addCategory(Intent.CATEGORY_OPENABLE);
                try{
                    startActivityForResult(Intent.createChooser(intent, "select a file"), REQUEST_1);
                } catch (android.content.ActivityNotFoundException ex){
                    Toast.makeText(MainActivity.this, "not found file manager", Toast.LENGTH_LONG).show();
                }
            }
        };
        filePicker.setOnClickListener(listener);

        Button piano = (Button) findViewById(R.id.paint_piano);
        View.OnClickListener listener1 = new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                Intent piano_intent = new Intent(MainActivity.this, PianoActivity.class);
                startActivity(piano_intent);
            }
        };
        piano.setOnClickListener(listener1);
    }

    @Override
    protected void onActivityResult(int requestcode, int resultCode, Intent intent) {
        super.onActivityResult(requestcode, resultCode, intent);
        if(requestcode == REQUEST_1) {
            if (resultCode == RESULT_OK) {
                Uri uri = intent.getData();
                String path = mpu.GetPath(this, uri);
                mpu.setPath(path);
                Log.d("alice_debug", "I can see the data here");
                //String data = mpu.ReadFile(path);
                if(!mpu.FindFrameStart(path)) {
                    Toast.makeText(MainActivity.this, "no frame found. Please pick up another file", Toast.LENGTH_SHORT).show();
                    return;
                }

                mp = new MediaPlayer();
                try {
                    Log.d("alice_debug", "I am in the music player");
                    mp.setDataSource(path);
                    mp.setLooping(false);
                    mp.prepare();
                    mp.start();
                    mp.setOnCompletionListener(new MediaPlayer.OnCompletionListener() {

                        @Override
                        public void onCompletion(MediaPlayer arg0) {
                            mp.release();
                            mp = null;
                        }

                    });
                } catch (IOException e) {
                    e.printStackTrace();
                }
                PaintView pv = new PaintView(this, mpu);
                setContentView(pv);

            }
        }

    }

}
