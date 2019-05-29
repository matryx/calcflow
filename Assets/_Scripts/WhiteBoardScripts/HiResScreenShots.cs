using UnityEngine;
using System.Collections;

public class HiResScreenShots : MonoBehaviour {
    public static HiResScreenShots Instance { get; private set; }
	public int resWidth = 2550; 
	public int resHeight = 3300;
    RenderTexture rt;
    private void Start()
    {
        if (Instance == null) Instance = this;
        rt = new RenderTexture(resWidth, resHeight, 24);
    }

    private bool takeHiResShot = false;
	public static string ScreenShotName(int width, int height) {
		return string.Format("{0}/screenshots/screen_{1}x{2}_{3}.png", 
			Application.dataPath, 
			width, height, 
			System.DateTime.Now.ToString("yyyy-MM-dd_HH-mm-ss"));
	}
	public void TakeHiResShot(string fileName) {
        fileName = fileName + ".png";
        byte[] bytes = GetScreenshotBytes(resWidth, resHeight);
        System.IO.File.WriteAllBytes(fileName, bytes);
        //Debug.Log(string.Format("Took screenshot to: {0}", fileName));
        takeHiResShot = false;
    }
    public byte[] GetScreenshotBytes(int width, int height)
    {
        RenderTexture temp = RenderTexture.active;
        Camera camera = GetComponent<Camera>();
        camera.targetTexture = rt;
        Texture2D screenShot = new Texture2D(resWidth, resHeight, TextureFormat.RGB24, false);
        camera.Render();
        RenderTexture.active = rt;
        screenShot.ReadPixels(new Rect(0, 0, resWidth, resHeight), 0, 0);
        camera.targetTexture = null;
        RenderTexture.active = temp; // JC: added to avoid errors
        byte[] bytes = screenShot.EncodeToPNG();
        return bytes;
    }
	void LateUpdate() {
		if (takeHiResShot) {

		}
	}

}