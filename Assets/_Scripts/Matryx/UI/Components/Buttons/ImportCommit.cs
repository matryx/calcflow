using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using UnityEngine;

using Matryx;

public class ImportCommit : QuickButton {

    public MatryxCommit commit;

    [SerializeField]
    private CalcManager calcManager;
    [SerializeField]
    private FlexButtonComponent flexButton;
    [SerializeField]
    private TMPro.TextMeshPro labelText;

    static int x = 0;
    protected override void ButtonEnterBehavior(GameObject other)
    {
        // Select the button
        if(flexButton.State != -1)
        {
            flexButton.SetState(2);
            var equationJson = JsonHelper.FromJson<SerializableExpressionSet>(commit.content);
            List<ExpressionSet> ess = equationJson.Select(x => x.ConvertToExpressionSet()).ToList();
            calcManager.LoadSavedExpressionSets(ess);
        }
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        // Deselect the button
        if(flexButton.State != -1)
        {
            flexButton.SetState(1);
        }
    }

    public void Disable(string text = "")
    {
        flexButton.SetState(-1);
        if (!text.Equals(""))
        {
            labelText.text = text;
        }
    }

    public void Enable(string text = "")
    {
        flexButton.SetState(0);
        if (!text.Equals(""))
        {
            labelText.text = text;
        }
    }
}
