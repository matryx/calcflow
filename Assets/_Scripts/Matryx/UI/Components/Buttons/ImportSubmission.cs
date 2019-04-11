using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using UnityEngine;

using Matryx;

public class ImportSubmission : QuickButton {

    public MatryxSubmission submission;

    [SerializeField]
    private CalcManager calcManager;
    [SerializeField]
    private FlexButtonComponent submissionButtonFlexComponent;

    int x = 0;
    protected override void ButtonEnterBehavior(GameObject other)
    {
        // Select the button
        submissionButtonFlexComponent.SetState(2);

        if(x%2 == 1)
        {
            var surface = CreateSubmissionMenu.Instance.SerializeSurface();
            Debug.Log("Surface: " + surface);
            var equationJson2 = JsonHelper.FromJson<SerializableExpressionSet>(surface);
            List<ExpressionSet> ess2 = equationJson2.Select(x => x.ConvertToExpressionSet()).ToList();
            CalcManager.Instance.LoadSavedExpressionSets(ess2);
        }
        else
        {
            var equationJson = JsonHelper.FromJson<SerializableExpressionSet>(submission.commit.content);
            List<ExpressionSet> ess = equationJson.Select(x => x.ConvertToExpressionSet()).ToList();
            calcManager.LoadSavedExpressionSets(ess);
        }

        x++;
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        // Deselect the button
        submissionButtonFlexComponent.SetState(1);
    }

    public void Disable()
    {
        submissionButtonFlexComponent.SetState(0);
    }

    public void Reenable()
    {
        submissionButtonFlexComponent.SetState(1);
    }
}
