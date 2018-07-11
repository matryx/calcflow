using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using TMPro;

public class LoadingScreen : MonoBehaviour
{

    TextMeshPro loadingText;
    bool loading;
    void Start()
    {
        loading = true;
        loadingText = GetComponentInChildren<TextMeshPro>();
        StartCoroutine(ManageText());
    }

	public void StopLoading(){
		loading = false;
	}

    IEnumerator ManageText()
    {
        int numDots = 0;
        while (loading)
        {
            numDots = (numDots + 1) % 4;
            string newText = "Loading";
            for (int i = 0; i < numDots; i++)
            {
                newText = newText + ".";
            }
            loadingText.text = newText;
            yield return new WaitForSeconds(1);
        }
    }
}
